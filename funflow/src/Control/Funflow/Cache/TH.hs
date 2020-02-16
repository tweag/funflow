{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

-- | Template Haskell splices for the funflow cache.
module Control.Funflow.Cache.TH where

import           Data.CAS.ContentStore
import           Data.Hashable
import           Language.Haskell.TH.Syntax
import           System.Random

-- | Create a default cacher with a random identity.
--
--   Note that this cacher is deliberately conservative - e.g.
--   if the application is recompiled, the cache will not be
--   reused.
defaultCacher :: Q Exp
defaultCacher = do
  (seed :: Int) <- runIO randomIO
  [e| defaultCacherWithIdent seed |]

instance Hashable Loc

-- | Create a default cacher based on the location of this splice.
--   Note that this may lead to invalid cacheing if the code is changed
--   without the version being updated.
defaultCacherLoc :: Int -- ^ Version
                 -> Q Exp
defaultCacherLoc ver = do
  loc <- location
  [e| defaultCacherWithIdent (hash (loc :: Loc, ver :: Int)) |]
