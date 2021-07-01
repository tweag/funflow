{-# LANGUAGE CPP #-}

-- | Generic file change notifier library for unix-based systems.
--
--   This library abstracts over specific implementations for BSD and linux
--   systems.
--
--   It provides facilities to watch specific directories for the following changes:
--   - File moves
--   - File deletion
--   - Attribute changes.
module Data.CAS.ContentStore.Notify
  ( Notifier,
    initNotifier,
    killNotifier,
    Watch,
    addDirWatch,
    removeDirWatch,
  )
where

#ifdef OS_Linux
import           Data.CAS.ContentStore.Notify.Linux
#else
#  ifdef OS_BSD
import           Data.CAS.ContentStore.Notify.BSD
#  endif
#endif
