{-# LANGUAGE EmptyDataDecls            #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE ViewPatterns              #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedLabels #-}

-- | Core Funflow types and functions.
--
--   In particular, you will probably care about the 'Flow' type, which is the
--   type of all Funflow workflows.
module Control.Funflow.Base where

import           Control.Arrow
import           Control.Arrow.Free
import           Control.Exception.Safe          (SomeException)
import           Control.Funflow.Diagram
import           Control.Funflow.External
import           Data.ByteString                 (ByteString)
import           Data.CAS.ContentHashable
import           Data.CAS.ContentStore           as CS
import           Data.Default
import           Data.Int                        (Int64)
import           Data.Proxy                      (Proxy (..))
import qualified Data.Text                       as T
import           Path
import           Prelude                         hiding (id, (.))
import           System.Random                   (randomIO)
import Control.Kernmantle.Rope


-- | Metadata writer
type MDWriter i o = Maybe (i -> o -> [(T.Text, ByteString)])

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

data CachedStep a b where
  Step :: Properties a b -> (a -> b) -> CachedStep a b
  StepIO :: Properties a b -> (a -> IO b) -> CachedStep a b

data ExternalStep a b where
  External :: ExternalProperties a
           -> (a -> ExternalTask)
           -> ExternalStep a CS.Item

data DirectStoreAccess a b where
  -- XXX: Constrain allowed user actions.
  PutInStore :: ContentHashable IO a
             => (Path Abs Dir -> a -> IO ())
             -> DirectStoreAccess a CS.Item
  -- XXX: Constrain allowed user actions.
  GetFromStore :: (Path Abs t -> IO a)
               -> DirectStoreAccess (CS.Content t) a
  InternalManipulateStore :: (CS.ContentStore -> a -> IO b)
                          -> DirectStoreAccess a b

type FunflowStrands = '[ '("externalStep", ExternalStep)
                       , '("cachedStep", CachedStep)
                       , '("directStoreAccess", DirectStoreAccess) ]

type Flow ex a b = TightRopeWith FunflowStrands
                                 '[TryEffect ex, ArrowChoice]
                                 a b

type RunnableFlow ex a b = forall core.
                            (TryEffect ex core, ArrowChoice core)
                         => TightRope FunflowStrands core a b

type SimpleFlow a b = forall core. (TryEffect SomeException core, ArrowChoice core)
                    => TightRope FunflowStrands core a b
type (==>) a b = SimpleFlow a b

-- | Convert a 'Flow' stripped of all extra effects to a diagram, for
-- inspection/pretty printing
toDiagram :: RunnableFlow ex a b -> Diagram ex a b
toDiagram flow = flow & loosen
                      & weave' #externalStep toDiagram'
                      & weave' #cachedStep stepToDiagram
                      & weave' #directStoreAccess toDiagram'
                      & untwine
  where
  stepToDiagram step = case step of
    Step (name -> Just n) f   -> node f [n]
    StepIO (name -> Just n) f -> node (Kleisli f) [n]
    _ -> error "toDiagram: should not happen"
  toDiagram' _
      = Node emptyNodeProperties (Proxy :: Proxy a1) (Proxy :: Proxy b1)
