{-# LANGUAGE CPP #-}

module Control.Funflow.ContentStore.Notify
  ( Notifier
  , initNotifier
  , killNotifier

  , Watch
  , addDirWatch
  , removeDirWatch
  ) where

#ifdef OS_Linux
import           Control.Funflow.ContentStore.Notify.Linux
#else
#  ifdef OS_BSD
import           Control.Funflow.ContentStore.Notify.BSD
#  endif
#endif
