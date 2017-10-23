{-# LANGUAGE Arrows, GADTs, OverloadedStrings, DeriveGeneric, ScopedTypeVariables #-}

module Control.FunFlow.Exec.Simple where

import qualified Data.Text as T
import Control.Exception (SomeException, catch)
import Data.IORef
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BS
import Control.Concurrent.MVar
import Data.Store
import Control.FunFlow.Base
import Data.Unique

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
runFlow :: Flow a b -> a -> IO b
runFlow f' x' = do po <- newLocalPostOffice
                   runFlow' po f' x'
  where
    runFlow' :: PostOffice -> Flow a b -> a -> IO b
    runFlow' _ (Step f) x = f x
    runFlow' po (Name _ f) x = runFlow' po f x
    runFlow' po (Compose f g) x = do
      y <- runFlow' po f x
      runFlow' po g y
    runFlow' po (First f) (x,d) = do
      y <- runFlow' po f x
      return (y,d)
    runFlow' _ (Arr f) x = return $ f x
    runFlow' po (Par f g) (x,y) = do
      w <- runFlow' po f x
      z <- runFlow' po g y
      return (w,z)
    runFlow' po (Fanin f _) (Left x) =
      runFlow' po f x
    runFlow' po (Fanin _ g) (Right x) =
      runFlow' po g x
    runFlow' po (Fold fstep) (lst, acc) = go lst acc where
      go [] y = return y
      go (x:xs) y0 = do
          y1 <- runFlow' po fstep (x,y0)
          go xs y1
    runFlow' po (Catch f h) x =
      runFlow' po f x `catch` (\e -> runFlow' po h (x,show (e::SomeException)))
    runFlow' po (Async ext) x = do
      mbox <- reserveMailBox po
      ext x po mbox
      Right y <- decode <$> awaitMail po mbox
      return y

