{-# LANGUAGE Arrows, GADTs, OverloadedStrings #-}

module Control.FunFlow.Base where

import Control.Arrow
import Control.Category
import Prelude hiding ((.), id)
import qualified Prelude
import Data.Aeson
import qualified Data.Text as T
import Control.Exception (SomeException, catch)

data Flow a b where
  Step    :: (FromJSON a,FromJSON b, ToJSON b) => (a -> IO b) -> Flow a b
  Arr     :: (a -> b) -> Flow a b
  Name    :: (FromJSON a,FromJSON b, ToJSON b) => T.Text -> Flow a b -> Flow a b
  Compose :: Flow a b -> Flow b c -> Flow a c
  First   :: Flow b c -> Flow (b,d) (c,d)
  Par     :: Flow b c -> Flow b' c' -> Flow (b, b') (c, c')
  Fanin   :: Flow b d -> Flow c d -> Flow (Either b c) d
  Fold    :: Flow (a,b) b -> Flow ([a],b) b
  Catch   :: Flow a b -> Flow (a,String) b -> Flow a b

instance Category Flow where
  id = Arr Prelude.id
  f . g = Compose g f

instance Arrow Flow where
  arr f = Arr f
  first  = First
  (***) = Par

instance Functor (Flow a) where
  fmap f flow = Compose flow (Arr f)

instance ArrowChoice Flow where
    left f = f +++ arr id
    right f = arr id +++ f
    f +++ g = (f >>> arr Left) ||| (g >>> arr Right)
    f ||| g = Fanin f g

(<:) :: (FromJSON a,FromJSON b, ToJSON b) => Flow a b -> T.Text -> Flow a b
f <: nm = Name nm f

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