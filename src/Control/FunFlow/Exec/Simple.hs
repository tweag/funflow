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
                                                      , Exception, try)
import           Path

-- | Simple evaulation of a flow
runFlowEx :: forall c eff ex a b. (Coordinator c, Exception ex)
          => c
          -> Config c
          -> Path Abs Dir -- ^ Path to content store
          -> (eff ~> AsyncA IO) -- ^ Natural transformation from wrapped effects
          -> Flow eff ex a b
          -> a
          -> IO b
runFlowEx _ cfg sroot runWrapped flow input = do
    hook <- initialise cfg
    CS.withStore sroot $ \store ->
      runAsyncA (eval (runFlow' hook store) flow) input
  where
    runFlow' :: Hook c -> CS.ContentStore -> Flow' eff a1 b1 -> AsyncA IO a1 b1
    runFlow' _ _ (Step f) = AsyncA $ \x -> f x
    runFlow' _ _ (Named _ f) = AsyncA $ \x -> return $ f x
    runFlow' po store (External toTask) = AsyncA $ \x -> do
      chash <- contentHash (x, toTask x)
      submitTask po $ TaskDescription chash (toTask x)
      KnownTask _ <- awaitTask po chash
      CS.waitUntilComplete store chash >>= \case
        Nothing -> fail "Remote process failed to construct item"
        Just item -> return item
    runFlow' _ store (PutInStore f) = AsyncA $ \x -> do
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
        CS.Missing fp -> do
          f fp x
          CS.markComplete store chash
    runFlow' _ _ (GetFromStore f) = AsyncA $ \item ->
      f $ CS.itemPath item
    runFlow' _ _ (Wrapped w) = runWrapped w

runFlow :: forall c eff ex a b. (Coordinator c, Exception ex)
        => c
        -> Config c
        -> Path Abs Dir -- ^ Path to content store
        -> (eff ~> AsyncA IO) -- ^ Natural transformation from wrapped effects
        -> Flow eff ex a b
        -> a
        -> IO (Either ex b)
runFlow c cfg sroot runWrapped flow input =
  try $ runFlowEx c cfg sroot runWrapped flow input

runSimpleFlow :: forall c a b. (Coordinator c)
        => c
        -> Config c
        -> Path Abs Dir -- ^ Path to content store
        -> SimpleFlow a b
        -> a
        -> IO (Either SomeException b)
runSimpleFlow c ccfg sroot flow input =
  runFlow c ccfg sroot runNoEffect flow input

-- | Create a full pipeline runner locally. This includes an executor for
--   executing external tasks.
--   This function is specialised to `SimpleFlow` since in cases where
--   a custom term algebra is in use, we assume that probably a centralised
--   coordinator and external runners may be desired as well.
withSimpleLocalRunner :: Path Abs Dir -- ^ Path to content store
                      -> ((SimpleFlow a b -> a -> IO (Either SomeException b))
                           -> IO c)
                      -> IO c
withSimpleLocalRunner storePath action = do
  memHook <- createMemoryCoordinator
  withAsync (executeLoop MemoryCoordinator memHook storePath) $ \_ ->
    action $ runSimpleFlow MemoryCoordinator memHook storePath
