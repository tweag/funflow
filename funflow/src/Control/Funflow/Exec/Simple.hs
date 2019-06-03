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
import           Control.Concurrent.Async                    (withAsync)
import           Control.Exception.Safe                      (Exception,
                                                              SomeException,
                                                              bracket,
                                                              onException,
                                                              throwM, try)
import           Control.Funflow.Base
import           Control.Funflow.ContentHashable
import qualified Control.Funflow.ContentStore                as CS
import           Control.Funflow.External
import           Control.Funflow.External.Coordinator
import           Control.Funflow.External.Coordinator.Memory
import           Control.Funflow.External.Executor           (executeLoop)
import qualified Control.Funflow.RemoteCache                 as Remote
import           Control.Monad.Catch                         (MonadCatch,
                                                              MonadMask)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control                 (MonadBaseControl)
import qualified Data.ByteString                             as BS
import           Data.Foldable                               (traverse_)
import           Data.Maybe
import           Data.Monoid                                 ((<>))
import           Data.Void
import           Katip
import           Path
import           System.IO                                   (stderr)

-- | Simple evaulation of a flow
runFlowEx :: forall m c eff ex a b remoteCache.
             (Coordinator c, Exception ex, MonadIO m, MonadBaseControl IO m
             ,MonadCatch m, MonadMask m, KatipContext m, Remote.Cacher m remoteCache)
          => c
          -> Config c
          -> CS.ContentStore
          -> remoteCache
          -> (eff ~> AsyncA m) -- ^ Natural transformation from wrapped effects
          -> Maybe Int -- ^ Flow configuration identity. This forms part of the
                       --   caching system and is used to disambiguate the same
                       --   flow run in multiple configurations. If Nothing,
                       --   then it means this flow has no identity, this
                       --   implies that steps will be executed without cache,
                       --   and external tasks will all be considered impure.
          -> Flow eff ex a b
          -> a
          -> m b
runFlowEx _ cfg store cacher runWrapped confIdent flow input = do
    hook <- initialise cfg
    runAsyncA (eval (runFlow' hook) flow) input
  where
    simpleOutPath item = toFilePath
      $ CS.itemPath store item </> [relfile|out|]
    withStoreCache :: forall i o. Cacher i o
                   -> AsyncA m i o -> AsyncA m i o
    withStoreCache c@Cache{} (AsyncA f)
      | Just confIdent' <- confIdent = AsyncA $ \i -> do
      let chash = cacherKey c confIdent' i
          computeAndStore fp = do
            res <- f i  -- Do the actual computation
            liftIO $ BS.writeFile (toFilePath $ fp </> [relfile|out|])
                   . cacherStoreValue c $ res
            return $ Right res
          readItem item = do
            bs <- liftIO . BS.readFile $ simpleOutPath item
            return . cacherReadValue c $ bs
      CS.withConstructIfMissing store cacher chash computeAndStore >>= \case
        CS.Missing e -> absurd e
        CS.Pending _ ->
          liftIO (CS.waitUntilComplete store chash) >>= \case
            Just item -> readItem item
            Nothing -> throwM $ CS.FailedToConstruct chash
        CS.Complete (Just a, _) -> return a
        CS.Complete (_, item) -> readItem item
    withStoreCache _ f = f
        
    writeMd :: forall i o. ContentHash
            -> i
            -> o
            -> MDWriter i o
            -> m ()
    writeMd _ _ _ Nothing = return ()
    writeMd chash i o (Just writer) =
      let kvs = writer i o
      in traverse_ (uncurry $ CS.setMetadata store chash) kvs


    runFlow' :: Hook c -> Flow' eff a1 b1 -> AsyncA m a1 b1
    runFlow' _ (Step props f) = withStoreCache (cache props)
      . AsyncA $ \x -> do
          let out = f x
          case cache props of
            Cache key _ _ | Just confIdent' <- confIdent ->
              writeMd (key confIdent' x) x out $ mdpolicy props
            _ -> return ()
          return out
    runFlow' _ (StepIO props f) = withStoreCache (cache props)
      . AsyncA $ liftIO . f
    runFlow' po (External props toTask) = AsyncA $ \x -> do
      let purity | Just _ <- confIdent = ep_impure props
                 | otherwise = alwaysRecompile
      chash <- liftIO $ case purity of
                          EpPure -> contentHash (toTask x)
                          EpImpure fn -> do
                            salt <- fn
                            contentHash (toTask x, salt)
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
            writeMd chash x () $ ep_mdpolicy props
            submitAndWait chash (TaskDescription chash (toTask x))
          -- Task is already known to the coordinator. Most likely something is
          -- running this task. Just wait for it.
          KnownTask _ -> wait chash (TaskDescription chash (toTask x))
        -- Nothing in the store. Submit and run.
        CS.Missing _ -> do
          writeMd chash x () $ ep_mdpolicy props
          submitAndWait chash (TaskDescription chash (toTask x))
      where
        submitAndWait chash td = do
          submitTask po td
          wait chash td
        wait chash td = do
          awaitTask po chash >>= \case
            KnownTask _ -> pure ()
            _ -> error "[Control.Funflow.Exec.Simple.runFlowEx] Expected KnownTask."
          CS.waitUntilComplete store chash >>= \case
            Just item -> return item
            Nothing -> do
              ti <- taskInfo po chash
              mbStdout <- CS.getMetadataFile store chash [relfile|stdout|]
              mbStderr <- CS.getMetadataFile store chash [relfile|stderr|]
              throwM $ ExternalTaskFailed td ti mbStdout mbStderr
    runFlow' _ (PutInStore f) = AsyncA $ \x -> katipAddNamespace "putInStore" $ do
      chash <- liftIO $ contentHash x
      CS.constructOrWait store cacher chash >>= \case
        CS.Pending void -> absurd void
        CS.Complete item -> return item
        CS.Missing fp ->
          do
            liftIO $ f fp x
            finalItem <- CS.markComplete store chash
            _ <- Remote.push cacher (CS.itemHash finalItem) (Just chash) (CS.itemPath store finalItem)
            pure finalItem
          `onException`
            (do $(logTM) WarningS . ls $ "Exception in construction: removing " <> show chash
                CS.removeFailed store chash
            )
    runFlow' _ (GetFromStore f) = AsyncA $ \case
      CS.All item -> liftIO . f $ CS.itemPath store item
      item CS.:</> path -> liftIO . f $ CS.itemPath store item </> path
    runFlow' _ (InternalManipulateStore f) = AsyncA $ \i -> liftIO $ f store i
    runFlow' _ (Wrapped props w) = withStoreCache (cache props)
      $ runWrapped w

-- | Run a flow in a logging context.
runFlowLog :: forall m c eff ex a b remoteCache.
              (Coordinator c, Exception ex, MonadIO m, MonadBaseControl IO m
              ,MonadCatch m, MonadMask m, KatipContext m, Remote.Cacher m remoteCache)
           => c
           -> Config c
           -> CS.ContentStore
           -> remoteCache
           -> (eff ~> AsyncA m) -- ^ Natural transformation from wrapped effects
           -> Maybe Int -- ^ Flow configuration identity. This forms part of the caching
                 --   system and is used to disambiguate the same flow run in
                 --   multiple configurations.
           -> Flow eff ex a b
           -> a
           -> m (Either ex b)
runFlowLog c cfg store cacher runWrapped confIdent flow input =
  try $ runFlowEx c cfg store cacher runWrapped confIdent flow input

-- | Run a flow, discarding all logging.
runFlow :: forall m c eff ex a b remoteCache.
           (Coordinator c, Exception ex, MonadIO m, MonadBaseControl IO m
           ,MonadCatch m, MonadMask m, Remote.Cacher (KatipContextT m) remoteCache)
        => c
        -> Config c
        -> CS.ContentStore
        -> remoteCache
        -> (eff ~> AsyncA m) -- ^ Natural transformation from wrapped effects
        -> Maybe Int -- ^ Flow configuration identity. This forms part of the caching
               --   system and is used to disambiguate the same flow run in
               --   multiple configurations.
        -> Flow eff ex a b
        -> a
        -> m (Either ex b)
runFlow c cfg store cacher runWrapped confIdent flow input = do
  le <- liftIO $ initLogEnv "funflow" "production"
  runKatipContextT le () "runFlow"
    $ runFlowLog c cfg store cacher (liftAsyncA . runWrapped) confIdent flow input

-- | Run a simple flow. Logging will be sent to stderr
runSimpleFlow :: forall m c a b.
                 (Coordinator c, MonadIO m, MonadBaseControl IO m
                 ,MonadCatch m, MonadMask m)
        => c
        -> Config c
        -> CS.ContentStore
        -> SimpleFlow a b
        -> a
        -> m (Either SomeException b)
runSimpleFlow c ccfg store flow input = do
  handleScribe <- liftIO $ mkHandleScribe ColorIfTerminal stderr InfoS V2
  let mkLogEnv = liftIO $
        registerScribe "stderr" handleScribe defaultScribeSettings =<< initLogEnv "funflow" "production"
  bracket mkLogEnv (liftIO . closeScribes) $ \le -> do
    let initialContext = ()
        initialNamespace = "executeLoop"

    runKatipContextT le initialContext initialNamespace
      $ runFlowLog c ccfg store Remote.NoCache runNoEffect (Just 12345) flow input

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
