{-# LANGUAGE Arrows              #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.FunFlow.Exec.Simple where

import           Control.Arrow                        (Kleisli (..), runKleisli)
import           Control.Arrow.Free                   (eval)
import           Control.FunFlow.Base
import           Control.FunFlow.ContentHashable
import qualified Control.FunFlow.ContentStore         as CS
import           Control.FunFlow.External
import           Control.FunFlow.External.Coordinator
import           Control.Monad.Catch                  (Exception, try)
import qualified Data.ByteString                      as BS
import           Data.Store                           (decode, encode)
import           System.FilePath                      ((</>))

-- | Simple evaulation of a flow
runFlow :: forall c ex a b. (Coordinator c, Exception ex)
        => c
        -> Config c
        -> FilePath -- ^ Path to content store
        -> Flow ex a b
        -> a
        -> IO (Either ex b)
runFlow _ cfg sroot flow input = do
    hook <- initialise cfg
    store <- CS.initialize sroot
    try $ runKleisli (eval (runFlow' hook store) flow) input
  where
    runFlow' :: Hook c -> CS.ContentStore -> Flow' a1 b1 -> Kleisli IO a1 b1
    runFlow' _ _ (Step f) = Kleisli $ \x -> f x
    runFlow' _ _ (Named _ f) = Kleisli $ \x -> return $ f x
    runFlow' po _ (External toTask) = Kleisli $ \x -> do
      chash <- contentHash (x, toTask x)
      submitTask po $ TaskDescription chash (toTask x)
      KnownTask _ <- awaitTask po chash
      return chash
    runFlow' _ store PutInStore = Kleisli $ \x -> do
      chash <- contentHash x
      instruction <- CS.constructIfMissing store chash
      case instruction of
        CS.Wait -> return chash -- XXX Correct behaviour?
        CS.Consume _ -> return chash
        CS.Construct fp -> let
            file = fp </> "out"
          in do
            BS.writeFile file $ encode x
            CS.markComplete store chash
            return chash
    runFlow' _ store GetFromStore = Kleisli $ \chash -> do
      mfp <- CS.lookup store chash
      case mfp of
        Nothing -> return Nothing
        Just fp -> let
            file = fp </> "out"
          in do
            bs <- BS.readFile file
            case decode bs of
              Right res -> return $ Just res
              Left _    -> return Nothing
