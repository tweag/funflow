{-# LANGUAGE TypeOperators #-}

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
  , CS.Cacher(..)
  , CS.defaultCacherWithIdent
  , Cache.defaultCacher
  , Cache.defaultCacherLoc
    -- * Defines our primitive flow functions
  , Class.ArrowFlow(..)
  , Class.step
  , Class.stepIO
  , Class.wrap
  , CS.withStore
  , module Control.Funflow.Steps
  , module Control.Funflow.Exec.Simple
  , module Control.Funflow.External
  )
  where

import qualified Control.Funflow.Base         as Base
import qualified Control.Funflow.Cache.TH     as Cache
import qualified Control.Funflow.Class        as Class
import           Control.Funflow.Exec.Simple
import           Control.Funflow.External
import           Control.Funflow.Steps
import qualified Data.CAS.ContentStore        as CS
