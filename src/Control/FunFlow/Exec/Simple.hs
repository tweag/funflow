{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Control.FunFlow.Exec.Simple
  ( runFlow
  , runFlowEx
  , runSimpleFlow
  , withSimpleLocalRunner
  ) where

import           Control.Arrow.Async
import           Control.Arrow.Free                   (eval, type (~>))
import           Control.Concurrent.Async             (wait, withAsync)
import           Control.FunFlow.Base
import           Control.FunFlow.ContentHashable
import qualified Control.FunFlow.ContentStore         as CS
import           Control.FunFlow.External
import           Control.FunFlow.External.Coordinator
import           Control.FunFlow.External.Coordinator.Memory
import           Control.FunFlow.External.Executor    (executeLoop)
import           Control.Monad.Catch                  ( SomeException
                                                      , Exception, onException
                                                      , try)
import           Path

-- | Simple evaulation of a flow
runFlowEx :: forall c eff ex a b. (Coordinator c, Exception ex)
          => c
          -> Config c
          -> CS.ContentStore
          -> (eff ~> AsyncA IO) -- ^ Natural transformation from wrapped effects
          -> Flow eff ex a b
          -> a
          -> IO b
runFlowEx _ cfg store runWrapped flow input = do
    hook <- initialise cfg
    runAsyncA (eval (runFlow' hook) flow) input
  where
    runFlow' :: Hook c -> Flow' eff a1 b1 -> AsyncA IO a1 b1
    runFlow' _ (Step f) = AsyncA $ \x -> f x
    runFlow' _ (Named _ f) = AsyncA $ \x -> return $ f x
    runFlow' po (External toTask) = AsyncA $ \x -> do
      chash <- contentHash (x, toTask x)
      submitTask po $ TaskDescription chash (toTask x)
      KnownTask _ <- awaitTask po chash
      CS.waitUntilComplete store chash >>= \case
        Nothing -> fail "Remote process failed to construct item"
        Just item -> return item
    runFlow' _ (PutInStore f) = AsyncA $ \x -> do
      chash <- contentHash x
      instruction <- CS.constructOrWait store chash
      case instruction of
        CS.Pending a -> do
          update <- wait a
          case update of
            CS.Completed item -> return item
            CS.Failed ->
              -- XXX: Should we retry locally?
              fail "Remote process failed to construct item"
        CS.Complete item -> return item
        CS.Missing fp ->
          do
            f fp x
            CS.markComplete store chash
          `onException`
          CS.removeFailed store chash
    runFlow' _ (GetFromStore f) = AsyncA $ \item ->
      f $ CS.itemPath store item
    runFlow' _ (Wrapped w) = runWrapped w

runFlow :: forall c eff ex a b. (Coordinator c, Exception ex)
        => c
        -> Config c
        -> CS.ContentStore
        -> (eff ~> AsyncA IO) -- ^ Natural transformation from wrapped effects
        -> Flow eff ex a b
        -> a
        -> IO (Either ex b)
runFlow c cfg store runWrapped flow input =
  try $ runFlowEx c cfg store runWrapped flow input

runSimpleFlow :: forall c a b. (Coordinator c)
        => c
        -> Config c
        -> CS.ContentStore
        -> SimpleFlow a b
        -> a
        -> IO (Either SomeException b)
runSimpleFlow c ccfg store flow input =
  runFlow c ccfg store runNoEffect flow input

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
