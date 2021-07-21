{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Funflow.Flow.Orphans where

import Control.Kernmantle.Caching (ProvidesCaching, StoreWithId (StoreWithId), usingStore)
import Control.Kernmantle.Error (MonadMask)
import Control.Kernmantle.Parallel (PKleisli)
import Control.Kernmantle.Rope (Reader, mapReader_, mapSieve, type (~>))
import Control.Monad.IO.Class (MonadIO)
import Data.CAS.ContentStore as CS
import qualified Data.CAS.RemoteCache as Remote
import UnliftIO (MonadUnliftIO)

-- TODO: Should be moved upstream to tweag/kernmantle
instance
  (MonadIO m, MonadUnliftIO m, MonadMask m, Remote.Cacher m remoteCacher) =>
  ProvidesCaching (Reader (StoreWithId remoteCacher) ~> PKleisli m)
  where
  usingStore =
    mapReader_ $ \(StoreWithId store remoteCacher pipelineId) ->
      mapSieve $ \act input ->
        CS.cacheKleisliIO
          pipelineId
          (CS.defaultIOCacherWithIdent 1)
          store
          remoteCacher
          act
          input
