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

type PostOffice = IORef (Map.Map T.Text (MVar BS.ByteString))

newPostOffice :: IO PostOffice
newPostOffice = do
  newIORef (Map.empty)

reservePostBox :: PostOffice -> IO PostOffice
reservePostBox po = return po

goPostal :: Store a => MailBox -> a -> IO ()
goPostal mb x = return ()

-- | Simple evaulation of a flow
runFlow :: Flow a b -> a -> IO b
runFlow (Step f) x = f x
runFlow (Name _ f) x = runFlow f x
runFlow (Compose f g) x = do
  y <- runFlow f x
  runFlow g y
runFlow (First f) (x,d) = do
  y <- runFlow f x
  return (y,d)
runFlow (Arr f) x = return $ f x
runFlow (Par f g) (x,y) = do
  w <- runFlow f x
  z <- runFlow g y
  return (w,z)
runFlow (Fanin f _) (Left x) =
  runFlow f x
runFlow (Fanin _ g) (Right x) =
  runFlow g x
runFlow (Fold fstep) (lst, acc) = go lst acc where
  go [] y = return y
  go (x:xs) y0 = do
      y1 <- runFlow fstep (x,y0)
      go xs y1
runFlow (Catch f h) x =
  runFlow f x `catch` (\e -> runFlow h (x,show (e::SomeException)))
runFlow (Async ext) x = do
  undefined

