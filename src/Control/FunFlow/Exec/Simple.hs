{-# LANGUAGE Arrows              #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
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
import qualified Data.ByteString                      as BS
import           Data.Store                           (decode, encode)
import           System.FilePath                      ((</>))

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
    runFlow' po _ (External toTask) = Kleisli $ \x -> do
      chash <- contentHash (x, toTask x)
      submitTask po $ TaskDescription chash (toTask x)
      KnownTask _ <- awaitTask po chash
      return chash
    runFlow' _ store PutInStore = Kleisli $ \x -> do
      chash <- contentHash x
      instruction <- CS.constructOrWait store chash
      case instruction of
        CS.Pending a -> do
          update <- wait a
          case update of
            CS.Completed _ -> return chash
            CS.Failed ->
              -- XXX: Should we retry locally?
              fail "Remote process failed to construct item"
        CS.Complete _ -> return chash
        CS.Missing fp -> let
            file = fp </> "out"
          in do
            BS.writeFile file $ encode x
            _ <- CS.markComplete store chash
            return chash
    runFlow' _ store GetFromStore = Kleisli $ \chash -> do
      instruction <- CS.lookupOrWait store chash
      mItem <- case instruction of
        CS.Missing () -> return Nothing
        CS.Complete item -> return (Just item)
        CS.Pending a -> do
          update <- wait a
          case update of
            CS.Completed item -> return (Just item)
            CS.Failed ->
              -- XXX: Should we fail instead?
              return Nothing
      case mItem of
        Nothing -> return Nothing
        Just item -> let
            file = CS.itemPath item </> "out"
          in do
            bs <- BS.readFile file
            case decode bs of
              Right res -> return $ Just res
              Left _    -> return Nothing
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
