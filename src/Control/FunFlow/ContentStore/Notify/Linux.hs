{-# LANGUAGE LambdaCase #-}

module Control.FunFlow.ContentStore.Notify.Linux
  ( Notifier
  , initNotifier
  , killNotifier

  , Watch
  , addDirWatch
  , removeDirWatch
  ) where

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
removeDirWatch = removeWatch
