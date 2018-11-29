-- |
-- This module defines the remote caching mechanism of funflow which is used to
-- keep several funflow stores (possibly on different machines) in sync.
module Control.Funflow.RemoteCache
  ( Cacher(..)
  , PullResult(..), PushResult(..)
  , noCache
  ) where

import           Control.Funflow.ContentHashable
import           Path


-- |
-- The result of a tentative pull from the remote cache
data PullResult
  = PullOK
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
  , pull :: ContentHash -> Path Abs Dir -> m PullResult
  }

-- |
-- A dummy remote cache implementation which does nothing
noCache :: Monad m => Cacher m
noCache = Cacher (\_ _ -> pure PushOK) (\_ _ -> pure NotInCache)
