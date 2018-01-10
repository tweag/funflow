{-# LANGUAGE Arrows              #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module Control.FunFlow.Exec.Simple
  ( runFlow
  , runFlowEx
  , runSimpleFlow
  , withSimpleLocalRunner
  ) where

import           Control.Arrow                               (returnA)
import           Control.Arrow.Async
import           Control.Arrow.Free                          (eval, type (~>))
import           Control.Concurrent.Async                    (withAsync)
import           Control.FunFlow.Base
import           Control.FunFlow.ContentHashable
import qualified Control.FunFlow.ContentStore                as CS
import           Control.FunFlow.External
import           Control.FunFlow.External.Coordinator
import           Control.FunFlow.External.Coordinator.Memory
import           Control.FunFlow.External.Executor           (executeLoop)
import           Control.Monad.Catch                         (Exception,
                                                              SomeException,
                                                              bracket,
                                                              onException,
                                                              throwM, try)
import           Control.Monad.IO.Class                      (liftIO)
import           Control.Monad.Trans.Class                   (lift)
import qualified Data.ByteString                             as BS
import           Data.Monoid                                 ((<>))
import           Data.Void
import           Katip
import           Path
import           System.IO                                   (stderr)

-- | Simple evaulation of a flow
runFlowEx :: forall c eff ex a b. (Coordinator c, Exception ex)
          => c
          -> Config c
          -> CS.ContentStore
          -> (eff ~> AsyncA (KatipContextT IO)) -- ^ Natural transformation from wrapped effects
          -> Int -- ^ Flow configuration identity. This forms part of the caching
                 --   system and is used to disambiguate the same flow run in
                 --   multiple configurations.
          -> Flow eff ex a b
          -> a
          -> KatipContextT IO b
runFlowEx _ cfg store runWrapped confIdent flow input = do
    hook <- initialise cfg
    runAsyncA (eval (runFlow' hook) flow) input
  where
    simpleOutPath item = toFilePath
      $ CS.itemPath store item </> [relfile|out|]
    withStoreCache :: forall i o. Cacher i o
                   -> AsyncA (KatipContextT IO) i o -> AsyncA (KatipContextT IO) i o
    withStoreCache NoCache f = f
    withStoreCache c f = let
        chashOf i = cacherKey c confIdent i
        checkStore = AsyncA $ \chash ->
          CS.constructOrWait store chash >>= \case
            CS.Pending void -> absurd void
            CS.Complete item -> do
              bs <- liftIO . BS.readFile $ simpleOutPath item
              return . Right . cacherReadValue c $ bs
            CS.Missing fp -> return $ Left fp
        writeStore = AsyncA $ \(chash, fp, res) ->
          do
             liftIO $ BS.writeFile (toFilePath $ fp </> [relfile|out|])
                         . cacherStoreValue c $ res
             _ <- CS.markComplete store chash
             return res
           `onException`
             CS.removeFailed store chash
      in proc i -> do
        let chash = chashOf i
        mcontents <- checkStore -< chash
        case mcontents of
          Right contents -> returnA -< contents
          Left fp -> do
            res <- f -< i
            writeStore -< (chash, fp, res)

    runFlow' :: Hook c -> Flow' eff a1 b1 -> AsyncA (KatipContextT IO) a1 b1
    runFlow' _ (Step props f) = withStoreCache (cache props)
      . AsyncA $ \x -> return $ f x
    runFlow' _ (StepIO props f) = withStoreCache (cache props)
      . liftAsyncA $ AsyncA f
    runFlow' po (External toTask) = AsyncA $ \x -> do
      chash <- liftIO $ contentHash (x, toTask x)
      CS.lookup store chash >>= \case
        -- The item in question is already in the store. No need to submit a task.
        CS.Complete item -> return item
        -- The item is pending in the store. In this case, we should check whether
        -- the coordinator knows about it
        CS.Pending _ -> taskInfo po chash >>= \case
          -- Something has gone wrong here. A task is marked as pending but the
          -- coordinator does not know about it. Attempt to remove the pending
          -- path and submit as normal.
          UnknownTask -> do
            CS.removeFailed store chash
            submitAndWait chash (TaskDescription chash (toTask x))
          -- Task is already known to the coordinator. Most likely something is
          -- running this task. Just wait for it.
          KnownTask _ -> wait chash (TaskDescription chash (toTask x))
        -- Nothing in the store. Submit and run.
        CS.Missing _ -> submitAndWait chash (TaskDescription chash (toTask x))
      where
        submitAndWait chash td = do
          submitTask po td
          wait chash td
        wait chash td = do
          KnownTask _ <- awaitTask po chash
          CS.waitUntilComplete store chash >>= \case
            Just item -> return item
            Nothing -> do
              ti <- taskInfo po chash
              mbStdout <- CS.getMetadataFile store chash [relfile|stdout|]
              mbStderr <- CS.getMetadataFile store chash [relfile|stderr|]
              throwM $ ExternalTaskFailed td ti mbStdout mbStderr
    runFlow' _ (PutInStore f) = AsyncA $ \x -> katipAddNamespace "putInStore" $ do
      chash <- liftIO $ contentHash x
      CS.constructOrWait store chash >>= \case
        CS.Pending void -> absurd void
        CS.Complete item -> return item
        CS.Missing fp ->
          do
            liftIO $ f fp x
            CS.markComplete store chash
          `onException`
            (do $(logTM) WarningS . ls $ "Exception in construction: removing " <> show chash
                CS.removeFailed store chash
            )
    runFlow' _ (GetFromStore f) = AsyncA $ \case
      CS.All item -> lift . f $ CS.itemPath store item
      item CS.:</> path -> lift . f $ CS.itemPath store item </> path
    runFlow' _ (InternalManipulateStore f) = AsyncA $ \i ->lift $ f store i
    runFlow' _ (Wrapped props w) = withStoreCache (cache props)
      $ runWrapped w

-- | Run a flow in a logging context.
runFlowLog :: forall c eff ex a b. (Coordinator c, Exception ex)
           => c
           -> Config c
           -> CS.ContentStore
           -> (eff ~> AsyncA (KatipContextT IO)) -- ^ Natural transformation from wrapped effects
           -> Int -- ^ Flow configuration identity. This forms part of the caching
                 --   system and is used to disambiguate the same flow run in
                 --   multiple configurations.
           -> Flow eff ex a b
           -> a
           -> KatipContextT IO (Either ex b)
runFlowLog c cfg store runWrapped confIdent flow input =
  try $ runFlowEx c cfg store runWrapped confIdent flow input

-- | Run a flow, discarding all logging.
runFlow :: forall c eff ex a b. (Coordinator c, Exception ex)
        => c
        -> Config c
        -> CS.ContentStore
        -> (eff ~> AsyncA IO) -- ^ Natural transformation from wrapped effects
        -> Int -- ^ Flow configuration identity. This forms part of the caching
               --   system and is used to disambiguate the same flow run in
               --   multiple configurations.
        -> Flow eff ex a b
        -> a
        -> IO (Either ex b)
runFlow c cfg store runWrapped confIdent flow input = do
  le <- initLogEnv "funflow" "production"
  runKatipContextT le () "runFlow"
    $ runFlowLog c cfg store (liftAsyncA . runWrapped) confIdent flow input

-- | Run a simple flow. Logging will be sent to stderr
runSimpleFlow :: forall c a b. (Coordinator c)
        => c
        -> Config c
        -> CS.ContentStore
        -> SimpleFlow a b
        -> a
        -> IO (Either SomeException b)
runSimpleFlow c ccfg store flow input = do
  handleScribe <- mkHandleScribe ColorIfTerminal stderr InfoS V2
  let mkLogEnv = registerScribe "stderr" handleScribe defaultScribeSettings =<< initLogEnv "funflow" "production"
  bracket mkLogEnv closeScribes $ \le -> do
    let initialContext = ()
        initialNamespace = "executeLoop"

    runKatipContextT le initialContext initialNamespace
      $ runFlowLog c ccfg store runNoEffect 12345 flow input

-- | Create a full pipeline runner locally. This includes an executor for
--   executing external tasks.
--   This function is specialised to `SimpleFlow` since in cases where
--   a custom term algebra is in use, we assume that probably a centralised
--   coordinator and external runners may be desired as well.
withSimpleLocalRunner :: Path Abs Dir -- ^ Path to content store
                      -> ((SimpleFlow a b -> a -> IO (Either SomeException b))
                           -> IO c)
                      -> IO c
withSimpleLocalRunner storePath action =
  CS.withStore storePath $ \store -> do
    memHook <- createMemoryCoordinator
    withAsync (executeLoop MemoryCoordinator memHook store) $ \_ ->
      action $ runSimpleFlow MemoryCoordinator memHook store
