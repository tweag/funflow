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
import           Control.FunFlow.External
import           Control.FunFlow.External.Coordinator
import           Control.Monad.Catch                  (Exception, try)

-- | Simple evaulation of a flow
runFlow :: forall c ex a b. (Coordinator c, Exception ex)
        => c
        -> Config c
        -> Flow ex a b
        -> a
        -> IO (Either ex b)
runFlow _ cfg flow input = do
  hook <- initialise cfg
  try $ runKleisli (eval (runFlow' hook) flow) input
  where
    runFlow' :: Hook c -> Flow' a1 b1 -> Kleisli IO a1 b1
    runFlow' _ (Step f) = Kleisli $ \x -> f x
    runFlow' _ (Named _ f) = Kleisli $ \x -> return $ f x
    runFlow' po (External toTask) = Kleisli $ \x -> do
      chash <- contentHash x
      submitTask po $ TaskDescription chash (toTask x)
      KnownTask _ <- awaitTask po chash
      return chash
