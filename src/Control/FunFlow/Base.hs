{-# LANGUAGE Arrows                    #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}

module Control.FunFlow.Base where

import           Control.Arrow.Free
import           Control.Category        ((.))
import           Control.FunFlow.Diagram
import           Data.ByteString         (ByteString)
import           Data.Proxy              (Proxy (..))
import           Data.Store
import qualified Data.Text               as T
import           GHC.Generics
import           Prelude                 hiding (id, (.))

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
  , send           :: MailBox -> ByteString -> IO () -- invoked by external
  , awaitMail      :: MailBox -> IO ByteString -- invoked by receive, blocking
  , checkMail      :: MailBox -> IO (Maybe ByteString) -- invoked by receive, nonblocking
  }

type External a b = a -> PostOffice -> MailBox -> IO ()

data Flow' a b where
  Step    :: Store b => (a -> IO b) -> Flow' a b
  Named   :: Store b => T.Text -> (a -> b) -> Flow' a b
  Async   :: Store b => External a b -> Flow' a b

type Flow = Choice Flow'

step :: Store b => (a -> IO b) -> Flow a b
step = effectChoice . Step

named :: Store b => T.Text -> (a -> b) -> Flow a b
named n f = effectChoice $ Named n f

-- | Convert a flow to a diagram, for inspection/pretty printing
toDiagram :: Flow a b -> Diagram a b
toDiagram flow = evalChoice toDiagram' flow where
  toDiagram' (Named n f)  = node f [n]
  toDiagram' _
      = Node emptyNodeProperties (Proxy :: Proxy a1) (Proxy :: Proxy b1)
