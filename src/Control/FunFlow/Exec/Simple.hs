{-# LANGUAGE Arrows              #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.FunFlow.Exec.Simple where

import           Control.Arrow           (Kleisli (..), runKleisli)
import           Control.Arrow.Free      (eval)
import           Control.Concurrent.MVar
import           Control.FunFlow.Base
import           Control.Monad.Catch     (Exception)
import qualified Data.ByteString         as BS
import           Data.IORef
import qualified Data.Map.Strict         as Map
import           Data.Store
import qualified Data.Text               as T
import           Data.Unique

newLocalPostOffice :: IO PostOffice
newLocalPostOffice = do
  lpo :: IORef (Map.Map T.Text (MVar BS.ByteString))
      <- newIORef Map.empty
  return $ PostOffice
    { reserveMailBox = do
        mv <- newEmptyMVar
        nm <- T.pack . show . hashUnique <$> newUnique
        modifyIORef lpo $ Map.insert nm mv
        return $ MailBox nm,
      send = \(MailBox nm) x-> do
        p <- readIORef lpo
        case Map.lookup nm p of
          Nothing -> do
            mv <- newMVar x
            modifyIORef lpo $ Map.insert nm mv
          Just mv ->
            putMVar mv x,
      awaitMail = \(MailBox nm)-> do
        p <- readIORef lpo
        case Map.lookup nm p of
          Nothing -> fail $ "no mailbox with name " ++ T.unpack nm
          Just mv ->
            takeMVar mv,
      checkMail = \(MailBox nm)-> do
        p <- readIORef lpo
        case Map.lookup nm p of
          Nothing -> return $ Nothing
          Just mv ->
            tryTakeMVar mv
    }

-- | Simple evaulation of a flow
runFlow :: Exception ex => Flow ex a b -> a -> IO b
runFlow flow input = do
  po <- newLocalPostOffice
  runKleisli (eval (runFlow' po) flow) input
  where
    runFlow' :: PostOffice -> Flow' a b -> Kleisli IO a b
    runFlow' _ (Step f) = Kleisli $ \x -> f x
    runFlow' _ (Named _ f) = Kleisli $ \x -> return $ f x
    runFlow' po (Async ext) = Kleisli $ \x -> do
      mbox <- reserveMailBox po
      ext x po mbox
      Right y <- decode <$> awaitMail po mbox
      return y
