{-# LANGUAGE EmptyDataDecls            #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeOperators             #-}

module Control.FunFlow.Base where

import           Control.Arrow.Free
import           Control.Category                ((.))
import           Control.Exception               (SomeException)
import           Control.FunFlow.ContentHashable
import qualified Control.FunFlow.ContentStore    as CS
import           Control.FunFlow.Diagram
import           Control.FunFlow.External
import qualified Control.FunFlow.External.Docker as Docker
import           Data.Proxy                      (Proxy (..))
import           Data.Store
import qualified Data.Text                       as T
import           Path
import           Prelude                         hiding (id, (.))

data Flow' eff a b where
  Step    :: Store b => (a -> IO b) -> Flow' eff a b
  Named   :: Store b => T.Text -> (a -> b) -> Flow' eff a b
  External :: ContentHashable a => (a -> ExternalTask) -> Flow' eff a CS.Item
  -- XXX: Constrain allowed user actions.
  PutInStore :: ContentHashable a => (Path Abs Dir -> a -> IO ()) -> Flow' eff a CS.Item
  -- XXX: Constrain allowed user actions.
  GetFromStore :: ContentHashable a => (Path Abs Dir -> IO a) -> Flow' eff CS.Item a
  Wrapped :: eff a b -> Flow' eff a b

type Flow eff ex = ErrorChoice ex (Flow' eff)

data NoEffect a b

-- | Since there are no constructors for 'NoEffect', this code can never be
--   reached and so is fine.
runNoEffect :: forall arr. NoEffect ~> arr
runNoEffect = error "Impossible!"

type SimpleFlow = Flow NoEffect SomeException

step :: Store b => (a -> IO b) -> Flow eff ex a b
step = effect . Step

named :: Store b => T.Text -> (a -> b) -> Flow eff ex a b
named n f = effect $ Named n f

external :: ContentHashable a => (a -> ExternalTask) -> Flow eff ex a CS.Item
external = effect . External

wrap :: eff a b -> Flow eff ex a b
wrap = effect . Wrapped

docker :: ContentHashable a => (a -> Docker.Config) -> Flow eff ex a CS.Item
docker f = external $ Docker.toExternal . f

putInStore :: ContentHashable a => (Path Abs Dir -> a -> IO ()) -> Flow eff ex a CS.Item
putInStore = effect . PutInStore
getFromStore :: ContentHashable a => (Path Abs Dir -> IO a) -> Flow eff ex CS.Item a
getFromStore = effect . GetFromStore


-- | Convert a flow to a diagram, for inspection/pretty printing
toDiagram :: Flow eff ex a b -> Diagram ex a b
toDiagram = eval toDiagram' where
  toDiagram' (Named n f)  = node f [n]
  toDiagram' _
      = Node emptyNodeProperties (Proxy :: Proxy a1) (Proxy :: Proxy b1)
