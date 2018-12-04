{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- This module defines the remote caching mechanism of funflow which is used to
-- keep several funflow stores (possibly on different machines) in sync.
module Control.Funflow.RemoteCache
  ( Cacher(..)
  , PullResult(..), PushResult(..), AliasResult(..)
  , NoCache(..), memoryCache
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

data AliasResult
 = AliasOK
 | TargetNotInCache
 | AliasError String

-- |
-- A simple mechanism for remote-caching.
--
-- Provides a way to push a path to the cache and pull it back.
--
-- No assumption is made on the availability of a store path. In particular,
-- pushing a path to the cache doesn't mean that we can pull it back.
class Monad m => Cacher m a where
  push ::
       a
    -> ContentHash -- ^ "Primary" key: hash of the content
    -> Maybe ContentHash -- ^ "Secondary" key: hash of the dependencies
    -> Path Abs Dir -- ^ Path to the content
    -> m PushResult
  pull :: a -> ContentHash -> Path Abs Dir -> m (PullResult ())

-- |
-- Push the path as an archive to the remote cache
pushAsArchive ::
     MonadIO m
  => (ContentHash -> ContentHash -> m (Either String ())) -- ^ How to create the aliases
  -> (ContentHash -> ByteString -> m PushResult) -- ^ How to push the content
  -> ContentHash -- ^ Primary key
  -> Maybe ContentHash -- ^ Secondary key
  -> Path Abs Dir
  -> m PushResult
pushAsArchive alias pushArchive primaryKey mSecondaryKey path = do
  archive <- liftIO $ Tar.write <$> Tar.pack (toFilePath path) ["."]
  pushArchive primaryKey archive >>= \case
    PushError e -> pure $ PushError e
    res ->
      case mSecondaryKey of
        Just secondaryKey ->
          alias primaryKey secondaryKey >>= \case
          Left err -> pure $ PushError err
          Right () -> pure res
        Nothing -> pure res

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
data NoCache = NoCache

instance Monad m => Cacher m NoCache where
  pull _ _ _ = pure NotInCache
  push _ _ _ _ = pure PushOK

-- |
-- An in-memory cache, for testing purposes
data MemoryCache = MemoryCache (MVar (Map ContentHash ByteString))
instance MonadIO m => Cacher m MemoryCache where
  pull (MemoryCache cacheVar) = pullAsArchive $ \hash -> do
    cacheMap <- liftIO $ readMVar cacheVar
    case Map.lookup hash cacheMap of
      Nothing -> pure NotInCache
      Just x  -> pure (PullOK x)
  push (MemoryCache cacheVar) = pushAsArchive alias $ \hash content -> do
    liftIO $ modifyMVar_
      cacheVar
      (\cacheMap -> pure $ Map.insert hash content cacheMap)
    pure PushOK
    where
      alias from to = liftIO $ Right <$> modifyMVar_ cacheVar
        (\cacheMap -> pure $ Map.insert to (cacheMap Map.! from) cacheMap)

memoryCache :: MonadIO m => m MemoryCache
memoryCache = liftIO $ MemoryCache <$> newMVar mempty
