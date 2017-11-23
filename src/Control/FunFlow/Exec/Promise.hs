{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Control.FunFlow.Exec.Promise
  ( runFlow
  , runFlowEx
  , runSimpleFlow
  , withSimpleLocalRunner
  ) where

import           Control.Arrow
import           Control.Arrow.Free                   (eval, type (~>))
import           Control.Concurrent.Async
import           Control.Arrow.Promise
import           Control.FunFlow.Base
import           Control.FunFlow.ContentHashable
import qualified Control.FunFlow.ContentStore         as CS
import           Control.FunFlow.External
import qualified Control.FunFlow.External.Coordinator as Coord
import           Control.FunFlow.External.Coordinator.Memory
import           Control.FunFlow.External.Executor    (executeLoop)
import           Control.Lens                         ((^.))
import           Control.Monad.Catch                  ( SomeException
                                                      , Exception, try
                                                      , onException )
import           Data.Monoid                          ((<>))
import qualified Data.Sequence                        as Seq
import qualified Data.Text                            as T
import           Path


chashInfo :: ContentHash -> T.Text
chashInfo = T.pack . toFilePath . hashToPath

awaitTaskA
  :: Coord.Coordinator c
  => Coord.Hook c
  -> PromiseA T.Text IO TaskDescription ContentHash
awaitTaskA po = kleisli "await task" $
  \(TaskDescription chash task) -> PromiseT $ do
    let info =
          "await task producing " <> chashInfo chash
          <> " (" <> task^.etCommand <> ")"
    promise info $ Coord.awaitTask po chash >>= \case
      Coord.UnknownTask -> error "Failed to submit task"
      Coord.KnownTask Coord.Pending -> error "Failed to await task"
      Coord.KnownTask (Coord.Running _) -> error "Failed to await task"
      Coord.KnownTask (Coord.Failed execInfo exitCode) ->
        return $ Failed (undefined execInfo exitCode)
      Coord.KnownTask (Coord.Completed _) ->
        return $ Ready chash

executeTaskA
  :: (ContentHashable a, Coord.Coordinator c)
  => Coord.Hook c
  -> CS.ContentStore
  -> (a -> ExternalTask)
  -> PromiseA T.Text IO a ContentHash
executeTaskA po store toTask = kleisli "submit task" $
  \a -> PromiseT $ do
    let task = toTask a
    chash <- contentHash (a, task)
    let desc = TaskDescription chash task
    CS.lookup store chash >>= \case
      CS.Complete _ -> return $ Ready chash
      CS.Pending () -> unPromiseT $ unPromiseA (awaitTaskA po) (Ready desc)
      CS.Missing () -> do
        Coord.submitTask po desc
        Coord.taskInfo po chash >>= \case
          Coord.UnknownTask -> error "Failed to submit task"
          Coord.KnownTask (Coord.Failed execInfo exitCode) ->
            return $ Failed (undefined execInfo exitCode)
          Coord.KnownTask (Coord.Completed _) ->
            return $ Ready chash
          Coord.KnownTask (Coord.Running _ ) ->
            unPromiseT $ unPromiseA (awaitTaskA po) (Ready desc)
          Coord.KnownTask Coord.Pending ->
            unPromiseT $ unPromiseA (awaitTaskA po) (Ready desc)

lookupItemA
  :: CS.ContentStore -> PromiseA T.Text IO ContentHash CS.Item
lookupItemA store = kleisli "lookup store item" $
  \chash -> PromiseT $ do
    CS.lookupOrWait store chash >>= \case
      CS.Missing () -> do
        return $ Failed (undefined chash)
      CS.Complete item -> do
        return $ Ready item
      CS.Pending async_upd -> do
        let info = "await store item " <> chashInfo chash
        promise info $ waitCatch async_upd >>= \case
          Left err ->
            return $ Failed (Seq.singleton err)
          Right CS.Failed ->
            return $ Failed (undefined chash)
          Right (CS.Completed item) ->
            return $ Ready item

putInStoreA
  :: ContentHashable a
  => CS.ContentStore
  -> (Path Abs Dir -> a -> IO ())
  -> PromiseA T.Text IO a CS.Item
putInStoreA store f = kleisli "put into store" $
  \a -> PromiseT $ do
    chash <- contentHash a
    CS.constructOrWait store chash >>= \case
      CS.Complete item -> return $ Ready item
      CS.Pending async_upd -> do
        let info = "put into store " <> chashInfo chash
        promise info $ waitCatch async_upd >>= \case
          Right (CS.Completed item) -> return $ Ready item
          Right CS.Failed -> return $ Failed (undefined chash)
          Left err -> return $ Failed (Seq.singleton err)
      CS.Missing fp ->
        do
          f fp a
          Ready <$> CS.markComplete store chash
        `onException`
        CS.removeFailed store chash

-- | Simple evaulation of a flow
runFlowEx :: forall c eff ex a b. (Coord.Coordinator c, Exception ex)
          => c
          -> Coord.Config c
          -> CS.ContentStore
          -> (eff ~> PromiseA T.Text IO) -- ^ Natural transformation from wrapped effects
          -> Flow eff ex a b
          -> a
          -> IO (Promise T.Text b)
runFlowEx _ cfg store runWrapped flow input = do
    hook <- Coord.initialise cfg
    unPromiseT $ unPromiseA (eval (runFlow' hook) flow) $ Ready input
  where
    runFlow' :: Coord.Hook c -> Flow' eff a1 b1 -> PromiseA T.Text IO a1 b1
    runFlow' _ (Step f) =
      kleisli "step" (PromiseT . fmap Ready . f)
    runFlow' _ (Named n f) =
      kleisli ("pure " <> n) (PromiseT . return . Ready . f)
    runFlow' po (External toTask) =
      executeTaskA po store toTask >>> lookupItemA store
    runFlow' _ (PutInStore f) =
      putInStoreA store f
    runFlow' _ (GetFromStore f) =
      kleisli "getFromStore" $ \item -> PromiseT $
        Ready <$> f (CS.itemPath item)
    runFlow' _ (Wrapped w) = runWrapped w

runFlow :: forall c eff ex a b. (Coord.Coordinator c, Exception ex)
        => c
        -> Coord.Config c
        -> CS.ContentStore
        -> (eff ~> PromiseA T.Text IO) -- ^ Natural transformation from wrapped effects
        -> Flow eff ex a b
        -> a
        -- XXX: Clean-up duplication between 'Left' and 'Failed'.
        -> IO (Either ex (Promise T.Text b))
runFlow c cfg store runWrapped flow input =
  try $ runFlowEx c cfg store runWrapped flow input

runSimpleFlow :: forall c a b. (Coord.Coordinator c)
        => c
        -> Coord.Config c
        -> CS.ContentStore
        -> SimpleFlow a b
        -> a
        -- XXX: Clean-up duplication between 'Left' and 'Failed'.
        -> IO (Either SomeException (Promise T.Text b))
runSimpleFlow c ccfg store flow input =
  runFlow c ccfg store runNoEffect flow input

-- | Create a full pipeline runner locally. This includes an executor for
--   executing external tasks.
--   This function is specialised to `SimpleFlow` since in cases where
--   a custom term algebra is in use, we assume that probably a centralised
--   coordinator and external runners may be desired as well.
withSimpleLocalRunner
  :: Path Abs Dir -- ^ Path to content store
  -> ((SimpleFlow a b -> a -> IO (Either SomeException (Promise T.Text b)))
  -> IO c)
  -> IO c
withSimpleLocalRunner storePath action =
  CS.withStore storePath $ \store -> do
    memHook <- createMemoryCoordinator
    withAsync (executeLoop MemoryCoordinator memHook store) $ \_ ->
      action $ runSimpleFlow MemoryCoordinator memHook store
