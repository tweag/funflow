{-# LANGUAGE EmptyDataDecls            #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE ViewPatterns              #-}

-- | Core Funflow types and functions.
--
--   In particular, you will probably care about the 'Flow' type, which is the
--   type of all Funflow workflows.
module Control.Funflow.Base where

import           Control.Arrow                   (Kleisli (..))
import           Control.Arrow.Free
import           Control.Exception.Safe          (SomeException)
import           Control.Funflow.ContentHashable
import qualified Control.Funflow.ContentStore    as CS
import           Control.Funflow.Diagram
import           Control.Funflow.External
import           Data.ByteString                 (ByteString)
import           Data.Default
import           Data.Functor.Identity
import           Data.Int                        (Int64)
import           Data.Proxy                      (Proxy (..))
import qualified Data.Store                      as Store
import qualified Data.Text                       as T
import           Path
import           Prelude                         hiding (id, (.))
import           System.Random                   (randomIO)

-- | Metadata writer
type MDWriter i o = Maybe (i -> o -> [(T.Text, ByteString)])

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

data EpPurity = EpPure | EpImpure (IO Int64)

instance Default EpPurity where
  def = EpPure

alwaysRecompile :: EpPurity
alwaysRecompile = EpImpure randomIO

-- | Additional properties associated with external tasks.
data ExternalProperties a = ExternalProperties
  { -- | Write additional metadata to the content store.
    ep_mdpolicy :: MDWriter a ()
    -- | Specify that this external step is impure, and as such should not be
    -- cached.
  , ep_impure :: EpPurity
  }

instance Default (ExternalProperties a) where
  def = ExternalProperties
    { ep_mdpolicy = Nothing
    , ep_impure = def
    }

data Flow' eff a b where
  Step :: Properties a b -> (a -> b) -> Flow' eff a b
  StepIO :: Properties a b -> (a -> IO b) -> Flow' eff a b
  External :: ExternalProperties a
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
type (==>) = SimpleFlow

-- | Convert a flow to a diagram, for inspection/pretty printing
toDiagram :: Flow eff ex a b -> Diagram ex a b
toDiagram = eval toDiagram' where
  toDiagram' (Step (name -> Just n) f)  = node f [n]
  toDiagram' (StepIO (name -> Just n) f)  = node (Kleisli f) [n]
  toDiagram' _
      = Node emptyNodeProperties (Proxy :: Proxy a1) (Proxy :: Proxy b1)
