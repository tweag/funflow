{-# LANGUAGE Arrows                    #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}

module Control.FunFlow.Base where

import           Control.Arrow.Free
import           Control.Category                ((.))
import           Control.FunFlow.ContentHashable
import           Control.FunFlow.Diagram
import           Control.FunFlow.External
import           Data.Proxy                      (Proxy (..))
import           Data.Store
import qualified Data.Text                       as T
import           Prelude                         hiding (id, (.))

data Flow' a b where
  Step    :: Store b => (a -> IO b) -> Flow' a b
  Named   :: Store b => T.Text -> (a -> b) -> Flow' a b
  External :: ContentHashable a => (a -> ExternalTask) -> Flow' a ContentHash
  PutInStore :: (ContentHashable a, Store a) => Flow' a ContentHash
  GetFromStore :: (ContentHashable a, Store a) => Flow' ContentHash (Maybe a)

type Flow ex = ErrorChoice ex Flow'

step :: Store b => (a -> IO b) -> Flow ex a b
step = effect . Step

named :: Store b => T.Text -> (a -> b) -> Flow ex a b
named n f = effect $ Named n f

external :: ContentHashable a => (a -> ExternalTask) -> Flow ex a ContentHash
external = effect . External

putInStore :: (ContentHashable a, Store a) => Flow ex a ContentHash
putInStore = effect $ PutInStore
getFromStore :: (ContentHashable a, Store a) => Flow ex ContentHash (Maybe a)
getFromStore = effect $ GetFromStore


-- | Convert a flow to a diagram, for inspection/pretty printing
toDiagram :: Flow ex a b -> Diagram ex a b
toDiagram flow = eval toDiagram' flow where
  toDiagram' (Named n f)  = node f [n]
  toDiagram' _
      = Node emptyNodeProperties (Proxy :: Proxy a1) (Proxy :: Proxy b1)
