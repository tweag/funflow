{-# LANGUAGE Arrows              #-}
{-# LANGUAGE DeriveGeneric       #-}
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
  ) where

import           Control.Arrow                        (Kleisli (..), runKleisli)
import           Control.Arrow.Free                   (eval, type (~>))
import           Control.Concurrent.Async             (wait)
import           Control.FunFlow.Base
import           Control.FunFlow.ContentHashable
import qualified Control.FunFlow.ContentStore         as CS
import           Control.FunFlow.External
import           Control.FunFlow.External.Coordinator
import           Control.Monad.Catch                  ( SomeException
                                                      , Exception, try)

-- | Simple evaulation of a flow
runFlowEx :: forall c eff ex a b. (Coordinator c, Exception ex)
          => c
          -> Config c
          -> FilePath -- ^ Path to content store
          -> (eff ~> Kleisli IO) -- ^ Natural transformation from wrapped effects
          -> Flow eff ex a b
          -> a
          -> IO b
runFlowEx _ cfg sroot runWrapped flow input = do
    hook <- initialise cfg
    CS.withStore sroot $ \store ->
      runKleisli (eval (runFlow' hook store) flow) input
  where
    runFlow' :: Hook c -> CS.ContentStore -> Flow' eff a1 b1 -> Kleisli IO a1 b1
    runFlow' _ _ (Step f) = Kleisli $ \x -> f x
    runFlow' _ _ (Named _ f) = Kleisli $ \x -> return $ f x
    runFlow' po store (External toTask) = Kleisli $ \x -> do
      chash <- contentHash (x, toTask x)
      submitTask po $ TaskDescription chash (toTask x)
      KnownTask _ <- awaitTask po chash
      CS.lookupOrWait store chash >>= \case
        CS.Missing _ -> fail "Remote process failed to construct item"
        CS.Pending a -> wait a >>= \case
          CS.Failed -> fail "Remote process failed to construct item"
          CS.Completed item -> return item
        CS.Complete item -> return item
    runFlow' _ store (PutInStore f) = Kleisli $ \x -> do
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
    runFlow' _ _ (GetFromStore f) = Kleisli $ \item ->
      f $ CS.itemPath item
    runFlow' _ _ (Wrapped w) = runWrapped w

runFlow :: forall c eff ex a b. (Coordinator c, Exception ex)
        => c
        -> Config c
        -> FilePath -- ^ Path to content store
        -> (eff ~> Kleisli IO) -- ^ Natural transformation from wrapped effects
        -> Flow eff ex a b
        -> a
        -> IO (Either ex b)
runFlow c cfg sroot runWrapped flow input =
  try $ runFlowEx c cfg sroot runWrapped flow input

runSimpleFlow :: forall c a b. (Coordinator c)
        => c
        -> Config c
        -> FilePath -- ^ Path to content store
        -> SimpleFlow a b
        -> a
        -> IO (Either SomeException b)
runSimpleFlow c ccfg sroot flow input =
  runFlow c ccfg sroot runNoEffect flow input
