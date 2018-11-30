{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

-- |
-- This module defines the remote caching mechanism of funflow which is used to
-- keep several funflow stores (possibly on different machines) in sync.
module Control.Funflow.RemoteCache
  ( Cacher(..)
  , PullResult(..), PushResult(..)
  , noCache, memoryCache
  , pullAsArchive, pushAsArchive
  ) where

import qualified Codec.Archive.Tar               as Tar
import           Control.Concurrent.MVar
import           Control.Funflow.ContentHashable
import           Control.Monad.IO.Class          (MonadIO, liftIO)
import           Data.ByteString.Lazy            (ByteString)
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as Map
import           Path


-- |
-- The result of a tentative pull from the remote cache
data PullResult a
  = PullOK a
  | NotInCache
  | PullError String
  deriving (Eq, Ord, Show)

-- |
-- The result of a tentative push to the remote cache
data PushResult
  = PushOK
  | PushError String
  deriving (Eq, Ord, Show)

-- |
-- A simple mechanism for remote-caching.
--
-- Provides a way to push a path to the cache and pull it back.
--
-- No assumption is made on the availability of a store path. In particular,
-- pushing a path to the cache doesn't mean that we can pull it back.
data Cacher m = Cacher
  { push :: ContentHash -> Path Abs Dir -> m PushResult
  , pull :: ContentHash -> Path Abs Dir -> m (PullResult ())
  }

-- |
-- Push the path as an archive to the remote cache
pushAsArchive ::
     MonadIO m
  => (ContentHash -> ByteString -> m PushResult)
  -> ContentHash
  -> Path Abs Dir
  -> m PushResult
pushAsArchive pushArchive hash path = do
  archive <- liftIO $ Tar.write <$> Tar.pack (toFilePath path) ["."]
  pushArchive hash archive

pullAsArchive ::
     MonadIO m
  => (ContentHash -> m (PullResult ByteString))
  -> ContentHash
  -> Path Abs Dir
  -> m (PullResult ())
pullAsArchive pullArchive hash path =
  pullArchive hash >>= \case
    PullOK archive -> do
      liftIO $ Tar.unpack (toFilePath path) $ Tar.read archive
      pure $ PullOK ()
    NotInCache -> pure NotInCache
    PullError e -> pure $ PullError e

-- |
-- A dummy remote cache implementation which does nothing
noCache :: Monad m => Cacher m
noCache = Cacher (\_ _ -> pure PushOK) (\_ _ -> pure NotInCache)

-- |
-- An in-memory cache, for testing purposes
memoryCache :: MonadIO m => m (Cacher m)
memoryCache = do
  cacheVar <- liftIO $ newMVar (mempty :: Map ContentHash ByteString)
  let
    push = pushAsArchive $ \hash content -> do
      liftIO $ modifyMVar_
        cacheVar
        (\cacheMap -> pure $ Map.insert hash content cacheMap)
      pure PushOK
    pull = pullAsArchive $ \hash -> do
      cacheMap <- liftIO $ readMVar cacheVar
      case Map.lookup hash cacheMap of
        Nothing -> pure NotInCache
        Just x  -> pure (PullOK x)
  pure $ Cacher { push, pull }
