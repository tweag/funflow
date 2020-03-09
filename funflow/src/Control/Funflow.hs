{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE GADTs         #-}

-- | Central Funflow module.
--
--   This module just re-exports various other modules for convenience.
module Control.Funflow
  ( Base.Flow
  , Base.SimpleFlow
  , type (Base.==>)
  , Base.NoEffect
  , Base.Flow'(..)
  , Base.ExternalProperties(..)
  , Base.MDWriter
  , Base.Properties(..)
  , Base.EpPurity(..)
  , Base.alwaysRecompile
  , CS.CacherM(..)
  , CS.Cacher
  , CS.defaultCacherWithIdent
  , Cache.defaultCacher
  , Cache.defaultCacherLoc
    -- * Defines our primitive flow functions
  , Class.ArrowFlow(..)
  , Class.step
  , Class.stepIO
  , Class.wrap
  , CS.withStore
  , hoistFlowEff
  , expandFlowEff
  , module Control.Funflow.Steps
  , module Control.Funflow.Exec.Simple
  , module Control.Funflow.External
  )
  where

import           Control.Arrow.Free
import           Control.Funflow.Base         as Base
import qualified Control.Funflow.Cache.TH     as Cache
import qualified Control.Funflow.Class        as Class
import           Control.Funflow.Exec.Simple
import           Control.Funflow.External
import           Control.Funflow.Steps
import qualified Data.CAS.ContentStore        as CS

-- | Change the effect type present inside the 'Flow'
hoistFlowEff :: (forall x y. eff x y -> eff' x y) -> Flow eff ex a b -> Flow eff' ex a b
hoistFlowEff f = hoistErrorChoiceEff $ \e -> case e of
  Step p s -> Step p s
  StepIO p s -> StepIO p s
  External p s -> External p s
  PutInStore s -> PutInStore s
  GetFromStore s -> GetFromStore s
  InternalManipulateStore s -> InternalManipulateStore s
  Wrapped p s -> Wrapped p (f s)

-- | Turns each effect in the 'Flow' as a 'Flow' of some other effect
expandFlowEff :: (forall x y. Properties x y -> eff x y -> Flow eff' ex x y)
              -> Flow eff ex a b
              -> Flow eff' ex a b
expandFlowEff f = expandErrorChoiceEff $ \e -> case e of
  Step p s -> Class.step' p s
  StepIO p s -> Class.stepIO' p s
  External p s -> Class.external' p s
  PutInStore s -> Class.putInStore s
  GetFromStore s -> Class.getFromStore s
  InternalManipulateStore s -> Class.internalManipulateStore s
  Wrapped p s -> f p s
-- hoistFlowEff and expandFlowEff are here and not in Control.Funflow.Base to
-- avoid a dependency cycle
