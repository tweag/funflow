{-# LANGUAGE Arrows              #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

-- | This module contains the means to execute a pipeline.
--
--   You should probably start with 'withSimpleLocalRunner' and 'runSimpleFlow'.
--   The other functions in this module provide more flexible versions of
--   'runSimpleFlow'.
module Control.Funflow.Exec.Simple
  ( runFlow
  , runFlowEx
  , runSimpleFlow
  , withSimpleLocalRunner
  ) where

import           Control.Arrow                               (returnA)
import           Control.Arrow.Async
import           Control.Arrow.Free                          (type (~>), eval)
import           Control.Concurrent.Async.Lifted             (async, pollSTM,
                                                              withAsync)
import qualified Control.Concurrent.STM                      as STM
import           Control.Exception.Safe                      (Exception,
                                                              SomeException,
                                                              bracket,
                                                              onException,
                                                              throwM, try)
import           Control.Funflow.Base
import           Control.Funflow.ContentHashable
import qualified Control.Funflow.ContentStore                as CS
import           Control.Funflow.Exec.Progress               as Progress
import           Control.Funflow.External
import           Control.Funflow.External.Coordinator
import           Control.Funflow.External.Coordinator.Memory
import           Control.Funflow.External.Executor           (executeLoop)
import           Control.Monad.Fix                           (fix)
import           Control.Monad.IO.Class                      (liftIO)
import           Control.Monad.Trans.Class                   (lift)
import qualified Data.ByteString                             as BS
import           Data.Foldable                               (traverse_, for_)
import           Data.Int                                    (Int64)
import           Data.Monoid                                 ((<>))
import           Data.Void
import           Katip
import           Path
import           Streaming                                   (Of, Stream)
import qualified Streaming.Prelude                           as Streaming
import           System.IO                                   (stderr)
import           System.Random                               (randomIO)

-- | Flow execution monad.
--   Flows may:
--   - Execute actions in IO
--   - Write log messages
--   - Yield progress updates in a stream
type FlowM = Stream (Of Progress) (KatipContextT IO)

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
          -> FlowM b
runFlowEx _ cfg store runWrapped confIdent flow input = do
    hook <- initialise cfg
    progressChan :: STM.TChan Progress <- liftIO STM.newTChanIO
    runThread <- lift . async $ runAsyncA (eval (runFlow' hook progressChan) flow) input
    Streaming.untilRight . liftIO . STM.atomically . fix $ \retry -> do
      complete <- pollSTM runThread
      nextProgress <- STM.tryReadTChan progressChan
      case (complete, nextProgress) of
        -- If there is progress to report, we report it before returning the final value
        (_, Just pItem)             -> return $ Left pItem
        (Just (Right res), Nothing) -> return $ Right res
        (Just (Left err), Nothing)  -> throwM err
        (Nothing, Nothing)          -> retry
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
    writeMd :: forall i o.
               STM.TChan Progress.Progress
            -> Progress.NodeId
            -> Maybe ContentHash
            -> i
            -> o
            -> MDWriter i o
            -> (KatipContextT IO) ()
    writeMd _ _ _ _ _ Nothing = return ()
    writeMd pc nid mchash i o (Just writer) =
      let kvs = writer i o
      in for_ kvs $ \kv -> do
        for_ mchash $ \chash -> uncurry (CS.setMetadata store chash) kv
        writeProgress pc $ Progress.metadata nid kv


    writeProgress pc = liftIO . STM.atomically. STM.writeTChan pc
    doThingReportStatus progressChan nid thing = do
      writeProgress progressChan $ Progress.status nid Progress.Started
      res <- thing
      writeProgress progressChan $ Progress.status nid Progress.Finished
      return res

    runFlow' :: Hook c
             -> STM.TChan Progress
             -> Flow' eff a1 b1
             -> AsyncA (KatipContextT IO) a1 b1
    runFlow' _ progressChan (Step props f) = withStoreCache (cache props)
      . AsyncA $ \x -> do
          nid <- Progress.mkNodeId
          doThingReportStatus progressChan nid $ do
            let out = f x
            writeMd progressChan nid
              (case cache props of
                NoCache       -> Nothing
                Cache key _ _ -> Just $ key confIdent x
              ) x out $ mdpolicy props
            return out
    runFlow' _ progressChan (StepIO props f) = withStoreCache (cache props)
      . liftAsyncA $ AsyncA $ \x -> do
          nid <- mkNodeId
          doThingReportStatus progressChan nid $ f x
    runFlow' po progressChan (External props toTask) = AsyncA $ \x -> do
      nid <- Progress.mkNodeId
      chash <- liftIO $ if (ep_impure props)
               then do
                 salt <- randomIO :: IO Int64
                 contentHash (toTask x, salt)
               else contentHash (toTask x)

      writeProgress progressChan $ Progress.status nid Progress.Started
      writeProgress progressChan $ Progress.inputHash nid chash
      CS.lookup store chash >>= \case
        -- The item in question is already in the store. No need to submit a task.
        CS.Complete item -> do
          writeProgress progressChan $ Progress.status nid Progress.Cached
          return item
        -- The item is pending in the store. In this case, we should check whether
        -- the coordinator knows about it
        CS.Pending _ -> taskInfo po chash >>= \case
          -- Something has gone wrong here. A task is marked as pending but the
          -- coordinator does not know about it. Attempt to remove the pending
          -- path and submit as normal.
          UnknownTask -> do
            CS.removeFailed store chash
            writeMd progressChan nid (Just chash) x () $ ep_mdpolicy props
            submitAndWait nid chash (TaskDescription chash (toTask x))
          -- Task is already known to the coordinator. Most likely something is
          -- running this task. Just wait for it.
          KnownTask _ -> wait nid chash (TaskDescription chash (toTask x))
        -- Nothing in the store. Submit and run.
        CS.Missing _ -> do
          writeMd progressChan nid (Just chash) x () $ ep_mdpolicy props
          submitAndWait nid chash (TaskDescription chash (toTask x))
      where
        submitAndWait nid chash td = do
          submitTask po td
          wait nid chash td
        wait nid chash td = do
          KnownTask _ <- awaitTask po chash
          CS.waitUntilComplete store chash >>= \case
            Just item -> do
              writeProgress progressChan $ Progress.status nid Progress.Finished
              writeProgress progressChan $ Progress.outputHash nid (CS.itemHash item)
              return item
            Nothing -> do
              ti <- taskInfo po chash
              mbStdout <- CS.getMetadataFile store chash [relfile|stdout|]
              mbStderr <- CS.getMetadataFile store chash [relfile|stderr|]
              writeProgress progressChan $ Progress.status nid Progress.Errored
              throwM $ ExternalTaskFailed td ti mbStdout mbStderr
    runFlow' _ _ (PutInStore f) = AsyncA $ \x -> katipAddNamespace "putInStore" $ do
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
    runFlow' _ _ (GetFromStore f) = AsyncA $ \case
      CS.All item -> lift . f $ CS.itemPath store item
      item CS.:</> path -> lift . f $ CS.itemPath store item </> path
    runFlow' _ _ (InternalManipulateStore f) = AsyncA $ \i -> lift $ f store i
    runFlow' _ _ (Wrapped props w) = withStoreCache (cache props)
      $ runWrapped w

-- | Run a flow in a logging context, converting progress updates to log messages.
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
  try $ Streaming.mapM_ (logFM InfoS . ls . show)
      $ runFlowEx c cfg store runWrapped confIdent flow input

-- | Run a flow, discarding all logging and progress updates.
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
