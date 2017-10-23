{-# LANGUAGE Arrows, GADTs, OverloadedStrings, DeriveGeneric, ExistentialQuantification #-}

module Control.FunFlow.Base where

import Control.Arrow
import Control.Category
import Prelude hiding ((.), id)
import qualified Prelude
import Data.Store
import qualified Data.Text as T
import GHC.Generics
import Data.ByteString (ByteString)


newtype MailBox = MailBox { unMailBox :: T.Text }
  deriving Generic

-- | a general facility for sending msgs. Can be created in different ways,
-- see e.g. `newLocalPostOffice`.
-- Some PostOffices may survive restart (e.g. backed by persistency). Of course you
-- wont get the same object but the MailBox will in that case still be
-- be routed to the right recipient. Other PostOffices will not survive process
-- restart.
data PostOffice = PostOffice
  { reserveMailBox :: IO MailBox -- invoked by receiver
  , send :: MailBox -> ByteString -> IO () -- invoked by external
  , awaitMail :: MailBox -> IO ByteString -- invoked by receive, blocking
  , checkMail :: MailBox -> IO (Maybe ByteString) -- invoked by receive, nonblocking
  }

type External a b = a -> PostOffice -> MailBox -> IO ()

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
  Async   :: Store b => External a b -> Flow a b

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

