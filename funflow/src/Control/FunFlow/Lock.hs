-- | Thread and process write lock.
--
-- Allows synchronisation between threads and processes.
-- Uses an 'MVar' for synchronisation between threads
-- and fcntl write locks for synchronisation between processes.
--
-- Only ever have one 'Lock' object per lock file per process!
module Control.FunFlow.Lock
  ( Lock
  , openLock
  , closeLock
  , withLock
  ) where

import           Control.Concurrent.MVar
import           Control.Exception
import           GHC.IO.Device           (SeekMode (AbsoluteSeek))
import           Path
import           System.Posix.Files
import           System.Posix.IO
import           System.Posix.Types

-- | Thread and process write lock.
--
-- Only ever have one 'Lock' object per lock file per process!
data Lock = Lock
  { lockMVar :: MVar ()
  , lockFd   :: Fd
  }

-- | Open the lock file and create a lock object.
--
-- This does not acquire the lock.
--
-- Only ever have one 'Lock' object per lock file per process!
openLock :: Path Abs File -> IO Lock
openLock lockFile = do
  mvar <- newMVar ()
  fd <- createFile (fromAbsFile $ lockFile) ownerWriteMode
  return $! Lock
    { lockMVar = mvar
    , lockFd = fd
    }

-- | Close the lock file.
--
-- Does not release the lock.
--
-- Blocks if the lock is taken.
closeLock :: Lock -> IO ()
closeLock lock = do
  takeMVar (lockMVar lock)
  closeFd (lockFd lock)

-- | Acquire the lock for the duration of the given action and release after.
withLock :: Lock -> IO a -> IO a
withLock lock action =
  withMVar (lockMVar lock) $ \() ->
    bracket_ (acquireFileLock $ lockFd lock) (releaseFileLock $ lockFd lock) $
      action

----------------------------------------------------------------------
-- Internals

makeLockDesc :: LockRequest -> FileLock
makeLockDesc req = (req, AbsoluteSeek, COff 0, COff 1)

acquireFileLock :: Fd -> IO ()
acquireFileLock fd = do
  let lockDesc = makeLockDesc WriteLock
  waitToSetLock fd lockDesc

releaseFileLock :: Fd -> IO ()
releaseFileLock fd = do
  let lockDesc = makeLockDesc Unlock
  setLock fd lockDesc
