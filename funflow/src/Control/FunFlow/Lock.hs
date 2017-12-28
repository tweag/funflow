{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

import           Control.Concurrent
import           Control.Exception
import           Control.Monad           (unless)
import           Network.HostName        (getHostName)
import           Path
import           Path.IO
import           System.Posix.Files
import           System.Posix.IO
import           System.Posix.Process
import           System.Random

-- | Thread and process write lock.
--
-- Only ever have one 'Lock' object per lock file per process!
data Lock = Lock
  { lockMVar :: MVar ()
  , lockDir  :: Path Abs Dir
  }

-- | Open the lock file and create a lock object.
--
-- This does not acquire the lock.
--
-- Only ever have one 'Lock' object per lock file per process!
openLock :: Path Abs Dir -> IO Lock
openLock dir = do
  mvar <- newMVar ()
  createDirIfMissing True dir
  return $! Lock
    { lockMVar = mvar
    , lockDir = dir
    }

-- | Close the lock file.
--
-- Does not release the lock.
--
-- Blocks if the lock is taken.
closeLock :: Lock -> IO ()
closeLock lock = do
  takeMVar (lockMVar lock)

-- | Acquire the lock for the duration of the given action and release after.
withLock :: Lock -> IO a -> IO a
withLock lock action =
  withMVar (lockMVar lock) $ \() ->
    bracket_ (acquireDirLock $ lockDir lock) (releaseDirLock $ lockDir lock) $
      action

----------------------------------------------------------------------
-- Internals

getUniqueFileName :: IO (Path Rel File)
getUniqueFileName = do
  hostName <- getHostName
  pid <- getProcessID
  parseRelFile $ hostName ++ show pid

lockFileName :: Path Rel File
lockFileName = [relfile|lock|]

acquireDirLock :: Path Abs Dir -> IO ()
acquireDirLock dir = do
  file <- getUniqueFileName
  let path = dir </> file
  fd <- createFile (fromAbsFile path) ownerWriteMode
  closeFd fd
  r <- try $ createLink (fromAbsFile path) (fromAbsFile $ dir </> lockFileName)
  case r of
    Right () -> return ()
    Left (_::IOError) -> do
      count <- linkCount <$> getFileStatus (fromAbsFile path)
      unless (count == 2) $ do
        delay <- randomRIO (50000, 100000)
        threadDelay delay
        acquireDirLock dir

releaseDirLock :: Path Abs Dir -> IO ()
releaseDirLock dir = do
  file <- getUniqueFileName
  let path = dir </> file
  removeLink (fromAbsFile $ dir </> lockFileName)
  removeLink (fromAbsFile path)
