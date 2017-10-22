{-# LANGUAGE Arrows, GADTs, OverloadedStrings, DeriveGeneric #-}

module Control.FunFlow.Exec.Simple where

import Control.Arrow
import qualified Data.Text as T
import Control.Exception (SomeException, catch)
import GHC.Generics
import Data.IORef
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BS
import Control.Concurrent.MVar
import Data.Store
import Control.FunFlow.Base
import Data.Unique

type PostOffice = IORef (Map.Map T.Text (MVar BS.ByteString))

newPostOffice :: IO PostOffice
newPostOffice = do
  newIORef (Map.empty)

reservePostBox :: PostOffice -> MailBox -> IO ()
reservePostBox po (MailBox nm) = do
  mv <- newEmptyMVar
  modifyIORef po $ Map.insert nm mv

goPostal :: Store a => PostOffice -> MailBox -> a -> IO ()
goPostal po (MailBox nm) x = do
  p <- readIORef po
  case Map.lookup nm p of
    Nothing -> do
      mv <- newMVar $ encode x
      modifyIORef po $ Map.insert nm mv
    Just mv ->
      putMVar mv $ encode x

checkMail :: Store a => PostOffice -> MailBox -> IO a
checkMail po (MailBox nm) = do
  p <- readIORef po
  case Map.lookup nm p of
    Nothing -> fail $ "no mailbox with name" ++ T.unpack nm
    Just mv ->
      let fromR (Right x) = x in
      fmap (fromR . decode) $ takeMVar mv


-- | Simple evaulation of a flow
runFlow :: Flow a b -> a -> IO b
runFlow f x = do po <- newPostOffice
                 runFlow' po f x
  where
    runFlow' :: PostOffice -> Flow a b -> a -> IO b
    runFlow' po (Step f) x = f x
    runFlow' po (Name _ f) x = runFlow' po f x
    runFlow' po (Compose f g) x = do
      y <- runFlow' po f x
      runFlow' po g y
    runFlow' po (First f) (x,d) = do
      y <- runFlow' po f x
      return (y,d)
    runFlow' po (Arr f) x = return $ f x
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
      nm <- T.pack . show . hashUnique <$> newUnique
      reservePostBox po (MailBox nm)
      undefined

