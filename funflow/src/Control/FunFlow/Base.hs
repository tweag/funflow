{-# LANGUAGE EmptyDataDecls            #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE ViewPatterns              #-}

module Control.FunFlow.Base where

import           Control.Arrow                   (Kleisli (..))
import           Control.Arrow.Free
import           Control.Exception.Safe          (SomeException)
import           Control.FunFlow.ContentHashable
import qualified Control.FunFlow.ContentStore    as CS
import           Control.FunFlow.Diagram
import           Control.FunFlow.External
import           Data.ByteString                 (ByteString)
import           Data.Default
import           Data.Functor.Identity
import           Data.Proxy                      (Proxy (..))
import qualified Data.Store                      as Store
import qualified Data.Text                       as T
import           Path
import           Prelude                         hiding (id, (.))

-- | Metadata writer
type MDWriter i o = Maybe (i -> o -> [(T.Text, T.Text)])

-- | A cacher is responsible for controlling how steps are cached.
data Cacher i o =
    NoCache -- ^ This step cannot be cached (default).
  | Cache
    { -- | Function to encode the input into a content
      --   hash.
      --   This function additionally takes an
      --   'identities' which gets incorporated into
      --   the cacher.
      cacherKey        :: Int -> i -> ContentHash
    , cacherStoreValue :: o -> ByteString
      -- | Attempt to read the cache value back. May throw exceptions.
    , cacherReadValue  :: ByteString -> o
    }

defaultCacherWithIdent :: (Store.Store o, ContentHashable Identity i)
                       => Int -- ^ Seed for the cacher
                       -> Cacher i o
defaultCacherWithIdent ident = Cache
  { cacherKey = \i ident' -> runIdentity $ contentHash (ident', ident, i)
  , cacherStoreValue = Store.encode
  , cacherReadValue = Store.decodeEx
  }

data Properties i o = Properties
  { -- | Name of this step. Used when describing the step in diagrams
    --   or other reporting.
    name     :: Maybe T.Text
    -- | Specify whether this step can be cached or not and, if so,
    --   how to do so.
  , cache    :: Cacher i o
    -- | Write additional metadata to the content store.
  , mdpolicy :: MDWriter i o
  }

instance Default (Properties i o) where
  def = Properties
    { name = Nothing
    , cache = NoCache
    , mdpolicy = Nothing
    }

-- | Additional properties associated with external tasks.
newtype ExternalProperties = ExternalProperties
  { -- | Write additional metadata to the content store.
    ep_mdpolicy :: MDWriter ExternalTask ()
  }

instance Default ExternalProperties where
  def = ExternalProperties
    { ep_mdpolicy = Nothing
    }

data Flow' eff a b where
  Step :: Properties a b -> (a -> b) -> Flow' eff a b
  StepIO :: Properties a b -> (a -> IO b) -> Flow' eff a b
  External :: ContentHashable IO a
           => ExternalProperties
           -> (a -> ExternalTask)
           -> Flow' eff a CS.Item
  -- XXX: Constrain allowed user actions.
  PutInStore :: ContentHashable IO a => (Path Abs Dir -> a -> IO ()) -> Flow' eff a CS.Item
  -- XXX: Constrain allowed user actions.
  GetFromStore :: (Path Abs t -> IO a) -> Flow' eff (CS.Content t) a
  -- Internally manipulate the store. This should not be used by
  -- client libraries.
  InternalManipulateStore :: (CS.ContentStore -> a -> IO b)
                          -> Flow' eff a b
  Wrapped :: Properties a b -> eff a b -> Flow' eff a b

type Flow eff ex = ErrorChoice ex (Flow' eff)

data NoEffect a b

-- | Since there are no constructors for 'NoEffect', this code can never be
--   reached and so is fine.
runNoEffect :: forall arr. NoEffect ~> arr
runNoEffect = error "Impossible!"

type SimpleFlow = Flow NoEffect SomeException

-- | Convert a flow to a diagram, for inspection/pretty printing
toDiagram :: Flow eff ex a b -> Diagram ex a b
toDiagram = eval toDiagram' where
  toDiagram' (Step (name -> Just n) f)  = node f [n]
  toDiagram' (StepIO (name -> Just n) f)  = node (Kleisli f) [n]
  toDiagram' _
      = Node emptyNodeProperties (Proxy :: Proxy a1) (Proxy :: Proxy b1)
