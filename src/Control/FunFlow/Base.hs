{-# LANGUAGE Arrows, GADTs, OverloadedStrings, DeriveGeneric #-}

module Control.FunFlow.Base where

import Control.Arrow
import Control.Category
import Prelude hiding ((.), id)
import qualified Prelude
import Data.Store
import qualified Data.Text as T
import GHC.Generics

newtype MailBox = MailBox { unMailBox :: T.Text }
  deriving Generic

type External a b = a -> MailBox -> IO ()

data Flow a b where
  Step    :: Store b => (a -> IO b) -> Flow a b
  Arr     :: (a -> b) -> Flow a b
  Name    :: Store b => T.Text -> Flow a b -> Flow a b
  Compose :: Flow a b -> Flow b c -> Flow a c
  First   :: Flow b c -> Flow (b,d) (c,d)
  Par     :: Flow b c -> Flow b' c' -> Flow (b, b') (c, c')
  Fanin   :: Flow b d -> Flow c d -> Flow (Either b c) d
  Fold    :: Flow (a,b) b -> Flow ([a],b) b
  Catch   :: Flow a b -> Flow (a,String) b -> Flow a b
  Async   :: External a b -> Flow a b

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

(<:) :: (Store a, Store b) => Flow a b -> T.Text -> Flow a b
f <: nm = Name nm f

