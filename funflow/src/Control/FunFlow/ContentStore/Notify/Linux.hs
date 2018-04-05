{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.FunFlow.ContentStore.Notify.Linux
  ( Notifier
  , initNotifier
  , killNotifier

  , Watch
  , addDirWatch
  , removeDirWatch
  ) where

import           Control.Exception.Safe (catch)
import           System.INotify

type Notifier = INotify

initNotifier :: IO Notifier
initNotifier = initINotify

killNotifier :: Notifier -> IO ()
killNotifier = killINotify

type Watch = WatchDescriptor

addDirWatch :: Notifier -> FilePath -> IO () -> IO Watch
addDirWatch inotify dir f = addWatch inotify mask dir $ \case
  Attributes True Nothing -> f
  MovedSelf True -> f
  DeletedSelf -> f
  _ -> return ()
  where
    mask = [Attrib, MoveSelf, DeleteSelf, OnlyDir]

removeDirWatch :: Watch -> IO ()
removeDirWatch w =
  -- When calling `addWatch` on a path that is already being watched,
  -- inotify will not create a new watch, but amend the existing watch
  -- and return the same watch descriptor.
  -- Therefore, the watch might already have been removed at this point,
  -- which will cause an 'IOError'.
  -- Fortunately, all event handlers to a file are called at once.
  -- So, that removing the watch here will not cause another handler
  -- to miss out on the event.
  -- Note, that this may change when adding different event handlers,
  -- that remove the watch under different conditions.
  removeWatch w
    `catch` \(_::IOError) -> return ()
