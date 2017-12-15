{-# LANGUAGE CPP #-}

module Control.FunFlow.ContentStore.Notify
  ( Notifier
  , initNotifier
  , killNotifier

  , Watch
  , addDirWatch
  , removeDirWatch
  ) where

#ifdef OS_Linux
import           Control.FunFlow.ContentStore.Notify.Linux
#else
#  ifdef OS_BSD
import           Control.FunFlow.ContentStore.Notify.BSD
#  endif
#endif
