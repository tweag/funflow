{-# LANGUAGE EmptyDataDecls            #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE ViewPatterns              #-}

module Control.FunFlow.Base where

import           Control.Arrow                   (Kleisli(..))
import           Control.Arrow.Free
import           Control.Category                ((.))
import           Control.Exception               (SomeException)
import           Control.FunFlow.ContentHashable
import qualified Control.FunFlow.ContentStore    as CS
import           Control.FunFlow.Diagram
import           Control.FunFlow.External
import qualified Control.FunFlow.External.Docker as Docker
import           Data.ByteString                 (ByteString)
import           Data.Default
import           Data.Functor.Identity
import           Data.Proxy                      (Proxy (..))
import qualified Data.Store as Store
import qualified Data.Text                       as T
import           Path
import           Prelude                         hiding (id, (.))

-- | A cacher is responsible for controlling how steps are cached.
data Cacher i o =
    NoCache -- ^ This step cannot be cached (default).
  | Cache
    { -- | A unique identifier included in the cache key to ensure
      --   that the same input does not give the same cache key for
      --   different steps.
      cacherUniqueIdent :: Int
      -- | Function to encode the input and the identity into a content
      --   hash.
    , cacherKey         :: i -> Int -> ContentHash
    , cacherStoreValue  :: o -> ByteString
      -- | Attempt to read the cache value back. May throw exceptions.
    , cacherReadValue   :: ByteString -> o
    }

defaultCacherWithIdent :: (Store.Store o, ContentHashable Identity i)
                       => Int -- ^ Seed for the cacher
                       -> Cacher i o
defaultCacherWithIdent ident = Cache
  { cacherUniqueIdent = ident
  , cacherKey = \i ident' -> runIdentity $ contentHash (ident', i)
  , cacherStoreValue = Store.encode
  , cacherReadValue = Store.decodeEx
  }

data Properties i o = Properties
  { -- | Name of this step. Used when describing the step in diagrams
    --   or other reporting.
    name  :: Maybe T.Text
    -- | Specify whether this step can be cached or not and, if so,
    --   how to do so.
  , cache :: Cacher i o
  }

instance Default (Properties i o) where
  def = Properties
    { name = Nothing
    , cache = NoCache
    }

data Flow' eff a b where
  Step :: Properties a b -> (a -> b) -> Flow' eff a b
  StepIO :: Properties a b -> (a -> IO b) -> Flow' eff a b
  External :: ContentHashable IO a => (a -> ExternalTask) -> Flow' eff a CS.Item
  -- XXX: Constrain allowed user actions.
  PutInStore :: ContentHashable IO a => (Path Abs Dir -> a -> IO ()) -> Flow' eff a CS.Item
  -- XXX: Constrain allowed user actions.
  GetFromStore :: ContentHashable IO a => (Path Abs Dir -> IO a) -> Flow' eff CS.Item a
  LookupAliasInStore :: Flow' eff CS.Alias (Maybe CS.Item)
  AssignAliasInStore :: Flow' eff (CS.Alias, CS.Item) ()
  Wrapped :: Properties a b -> eff a b -> Flow' eff a b

type Flow eff ex = ErrorChoice ex (Flow' eff)

data NoEffect a b

-- | Since there are no constructors for 'NoEffect', this code can never be
--   reached and so is fine.
runNoEffect :: forall arr. NoEffect ~> arr
runNoEffect = error "Impossible!"

type SimpleFlow = Flow NoEffect SomeException

step :: (a -> b) -> Flow eff ex a b
step = step' def

step' :: Properties a b -> (a -> b) -> Flow eff ex a b
step' props = effect . Step props

stepIO :: (a -> IO b) -> Flow eff ex a b
stepIO = stepIO' def

stepIO' :: Properties a b -> (a -> IO b) -> Flow eff ex a b
stepIO' props = effect . StepIO props

named :: T.Text -> (a -> b) -> Flow eff ex a b
named n = step' (def { name = Just n})

external ::  ContentHashable IO a => (a -> ExternalTask) -> Flow eff ex a CS.Item
external = effect . External

wrap :: eff a b -> Flow eff ex a b
wrap = effect . Wrapped def

wrap' :: Properties a b -> eff a b -> Flow eff ex a b
wrap' p eff = effect $ Wrapped p eff

docker ::  ContentHashable IO a => (a -> Docker.Config) -> Flow eff ex a CS.Item
docker f = external $ Docker.toExternal . f

putInStore ::  ContentHashable IO a => (Path Abs Dir -> a -> IO ()) -> Flow eff ex a CS.Item
putInStore = effect . PutInStore
getFromStore ::  ContentHashable IO a => (Path Abs Dir -> IO a) -> Flow eff ex CS.Item a
getFromStore = effect . GetFromStore

lookupAliasInStore :: Flow eff ex CS.Alias (Maybe CS.Item)
lookupAliasInStore = effect LookupAliasInStore
assignAliasInStore :: Flow eff ex (CS.Alias, CS.Item) ()
assignAliasInStore = effect AssignAliasInStore


-- | Convert a flow to a diagram, for inspection/pretty printing
toDiagram :: Flow eff ex a b -> Diagram ex a b
toDiagram = eval toDiagram' where
  toDiagram' (Step (name -> Just n) f)  = node f [n]
  toDiagram' (StepIO (name -> Just n) f)  = node (Kleisli f) [n]
  toDiagram' _
      = Node emptyNodeProperties (Proxy :: Proxy a1) (Proxy :: Proxy b1)
