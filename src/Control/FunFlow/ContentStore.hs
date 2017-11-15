{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Hash addressed store in file system.
--
-- A store associates a 'Control.FunFlow.ContentHashable.ContentHash'
-- with a directory subtree. A subtree can be either
-- 'Control.FunFlow.ContentStore.Missing',
-- 'Control.FunFlow.ContentStore.Pending, or
-- 'Control.FunFlow.ContentStore.Complete'.
-- The state of subtrees is persisted on the file system.
--
-- The store is thread-safe and multi-process safe.
--
-- It is assumed that the user that the process is running under is the owner
-- of the store root, or has permission to create it if missing.
--
-- It is assumed that the store root and its immediate contents are not modified
-- externally. The contents of subtrees may be modified externally while the
-- subtree is marked as pending.
--
-- __Implementation note:__
--
-- Two file-system features are used to persist the state of a subtree,
-- namely whether it exists and whether it is writable.
--
-- @
--   exists   writable    state
--   ----------------------------
--                       missing
--     X          X      pending
--     X                 complete
-- @
module Control.FunFlow.ContentStore
  ( Status (..)
  , Status_
  , Update (..)
  , StoreError (..)
  , ContentStore
  , Item
  , itemPath
  , root
  , open
  , close
  , withStore
  , allSubtrees
  , subtrees
  , subtreesPending
  , query
  , isMissing
  , isPending
  , isComplete
  , lookup
  , lookupOrWait
  , constructOrWait
  , constructIfMissing
  , markPending
  , markComplete
  , removeFailed
  , removeForcibly
  ) where


import           Prelude                         hiding (lookup)

import           Control.Concurrent              (threadDelay)
import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.Exception               (Exception, bracket_, catch,
                                                  throwIO)
import           Control.Monad                   (filterM, forever, void)
import           Control.Monad.Catch             (MonadMask, bracket)
import           Control.Monad.IO.Class          (MonadIO, liftIO)
import           Data.Bits                       (complement)
import           Data.List                       (foldl')
import           Data.Maybe                      (catMaybes)
import qualified Data.Store
import           Data.Typeable                   (Typeable)
import           GHC.Generics                    (Generic)
import           GHC.IO.Device                   (SeekMode (AbsoluteSeek))
import           System.Directory                (createDirectory,
                                                  createDirectoryIfMissing,
                                                  doesDirectoryExist,
                                                  listDirectory, makeAbsolute,
                                                  removePathForcibly)
import           System.FilePath                 ((</>))
import           System.INotify
import           System.Posix.Files
import           System.Posix.IO
import           System.Posix.Types

import           Control.FunFlow.ContentHashable (ContentHash,
                                                  ContentHashable (..),
                                                  DirectoryContent (..),
                                                  hashToPath, pathToHash)


-- | Status of a subtree in the store.
data Status missing pending complete
  = Missing missing
  -- ^ The subtree does not exist, yet.
  | Pending pending
  -- ^ The subtree is under construction and not ready for consumption.
  | Complete complete
  -- ^ The subtree is complete and ready for consumption.
  deriving (Eq, Show)

type Status_ = Status () () ()

-- | Update about the status of a pending subtree.
data Update
  = Completed Item
  -- ^ The item is now completed and ready for consumption.
  | Failed
  -- ^ Constructing the item failed.
  deriving (Eq, Show)

-- | Errors that can occur when interacting with the store.
data StoreError
  = NotPending ContentHash
  -- ^ A subtree is not under construction when it should be.
  | AlreadyPending ContentHash
  -- ^ A subtree is already under construction when it should be missing.
  | AlreadyComplete ContentHash
  -- ^ A subtree is already complete when it shouldn't be.
  deriving (Show, Typeable)
instance Exception StoreError

-- | A hash addressed store on the file system.
data ContentStore = ContentStore
  { storeRoot    :: FilePath
  -- ^ Subtrees are stored directly under this directory.
  , storeLock    :: MVar ()
  -- ^ One global lock on store metadata to ensure thread safety.
  -- The lock is taken when subtree state is changed or queried.
  , storeLockFd  :: Fd
  -- ^ One exclusive file lock to ensure multi-processing safety.
  -- Note, that file locks are shared between threads in a process,
  -- so that the file lock needs to be complemented by an `MVar`
  -- for thread-safety.
  , storeINotify :: INotify
  -- ^ Used to watch for updates on store items.
  }

-- | A completed item in the 'ContentStore'.
newtype Item = Item { itemPath :: FilePath }
  deriving (Eq, Show, Generic)

instance ContentHashable Item where
  contentHashUpdate ctx item =
    contentHashUpdate ctx (DirectoryContent (itemPath item))

instance Data.Store.Store Item

-- | The root directory of the store.
root :: ContentStore -> FilePath
root = storeRoot

-- | @open root@ opens a store under the given root directory.
--
-- The root directory is created if necessary.
--
-- It is not safe to have multiple store objects
-- refer to the same root directory.
open :: FilePath -> IO ContentStore
open root' = do
  storeRoot <- makeAbsolute root'
  createDirectoryIfMissing True storeRoot
  storeLockFd <- createFile (lockPath storeRoot) ownerWriteMode
  setFileMode storeRoot readOnlyRootDirMode
  storeLock <- newMVar ()
  storeINotify <- initINotify
  return ContentStore {..}

-- | Free the resources associated with the given store object.
--
-- The store object may not be used afterwards.
close :: ContentStore -> IO ()
close store = do
  takeMVar (storeLock store)
  closeFd (storeLockFd store)
  killINotify (storeINotify store)

-- | Open the under the given root and perform the given action.
-- Closes the store once the action is complete
--
-- See also: 'Control.FunFlow.ContentStore.open'
withStore :: (MonadIO m, MonadMask m) => FilePath -> (ContentStore -> m a) -> m a
withStore root' = bracket (liftIO $ open root') (liftIO . close)

-- | List all subtrees that are complete or under construction.
allSubtrees :: ContentStore -> IO [ContentHash]
allSubtrees ContentStore {storeRoot} =
  catMaybes . map pathToHash <$> listDirectory storeRoot

-- | List all complete subtrees.
subtrees :: ContentStore -> IO [ContentHash]
subtrees store = filterM (isComplete store) =<< allSubtrees store

-- | List all subtrees under construction.
subtreesPending :: ContentStore -> IO [ContentHash]
subtreesPending store =
  filterM (isPending store) =<< allSubtrees store

-- | Query for the state of a subtree.
query :: ContentStore -> ContentHash -> IO (Status () () ())
query store hash = withStoreLock store $
  internalQuery store hash

isMissing :: ContentStore -> ContentHash -> IO Bool
isMissing store hash = (== Missing ()) <$> query store hash

isPending :: ContentStore -> ContentHash -> IO Bool
isPending store hash = (== Pending ()) <$> query store hash

isComplete :: ContentStore -> ContentHash -> IO Bool
isComplete store hash = (== Complete ()) <$> query store hash

-- | Query a subtree and return it if completed.
lookup :: ContentStore -> ContentHash -> IO (Status () () Item)
lookup store hash = withStoreLock store $
  internalQuery store hash >>= \case
    Missing () -> return $ Missing ()
    Pending () -> return $ Pending ()
    Complete () -> return $ Complete (Item (storePath store hash))

-- | Query a subtree and return it if completed.
-- Return an 'Control.Concurrent.Async' to await updates,
-- if it is already under construction.
lookupOrWait
  :: ContentStore
  -> ContentHash
  -> IO (Status () (Async Update) Item)
lookupOrWait store hash = withStoreLock store $
  let dir = storePath store hash in
  internalQuery store hash >>= \case
    Complete () -> return $ Complete (Item dir)
    Missing () -> return $ Missing ()
    Pending () -> Pending <$> internalWatchPending store hash

-- | Atomically query the state of a subtree
-- and mark it as under construction if missing.
-- Return an 'Control.Concurrent.Async' to await updates,
-- if it is already under construction.
constructOrWait
  :: ContentStore
  -> ContentHash
  -> IO (Status FilePath (Async Update) Item)
constructOrWait store hash = withStoreLock store $
  let dir = storePath store hash in
  internalQuery store hash >>= \case
    Complete () -> return $ Complete (Item dir)
    Missing () -> withWritableStore store $ do
      createDirectory dir
      setDirWritable dir
      return $ Missing dir
    Pending () -> Pending <$> internalWatchPending store hash

-- | Atomically query the state of a subtree
-- and mark it as under construction if missing.
constructIfMissing
  :: ContentStore
  -> ContentHash
  -> IO (Status FilePath () Item)
constructIfMissing store hash = withStoreLock store $
  let dir = storePath store hash in
  internalQuery store hash >>= \case
    Complete () -> return $ Complete (Item dir)
    Pending () -> return $ Pending ()
    Missing () -> withWritableStore store $ do
      createDirectory dir
      setDirWritable dir
      return $ Missing dir

-- | Mark a non-existent subtree as under construction.
--
-- Creates the destination directory and returns its path.
--
-- See also: 'Control.FunFlow.ContentStore.constructIfMissing'.
markPending :: ContentStore -> ContentHash -> IO FilePath
markPending store hash = withStoreLock store $
  internalQuery store hash >>= \case
    Complete () -> throwIO (AlreadyComplete hash)
    Pending () -> throwIO (AlreadyPending hash)
    Missing () -> withWritableStore store $ do
      let dir = storePath store hash
      createDirectory dir
      setDirWritable dir
      return dir

-- | Mark a subtree that was under construction as complete.
markComplete :: ContentStore -> ContentHash -> IO Item
markComplete store hash = withStoreLock store $
  internalQuery store hash >>= \case
    Missing () -> throwIO (NotPending hash)
    Complete () -> throwIO (AlreadyComplete hash)
    Pending () -> withWritableStore store $ do
      let dir = storePath store hash
      unsetWritableRecursively dir
      return $ Item dir

-- | Remove a subtree that was under construction.
--
-- It is the callers responsibility to ensure that no other threads or processes
-- will attempt to access the subtree afterwards.
removeFailed :: ContentStore -> ContentHash -> IO ()
removeFailed store hash = withStoreLock store $
  internalQuery store hash >>= \case
    Missing () -> throwIO (NotPending hash)
    Complete () -> throwIO (AlreadyComplete hash)
    Pending () -> withWritableStore store $
      removePathForcibly (storePath store hash)

-- | Remove a subtree independent of its state.
-- Do nothing if it doesn't exist.
--
-- It is the callers responsibility to ensure that no other threads or processes
-- will attempt to access the subtree afterwards.
removeForcibly :: ContentStore -> ContentHash -> IO ()
removeForcibly store hash = withStoreLock store $ withWritableStore store $
  removePathForcibly (storePath store hash)


----------------------------------------------------------------------
-- Internals

lockPath :: FilePath -> FilePath
lockPath = (</> "lock")

makeLockDesc :: LockRequest -> FileLock
makeLockDesc req = (req, AbsoluteSeek, COff 0, COff 1)

acquireStoreFileLock :: ContentStore -> IO ()
acquireStoreFileLock ContentStore {storeLockFd} = do
  let lockDesc = makeLockDesc WriteLock
  waitToSetLock storeLockFd lockDesc

releaseStoreFileLock :: ContentStore -> IO ()
releaseStoreFileLock ContentStore {storeLockFd} = do
  let lockDesc = makeLockDesc Unlock
  setLock storeLockFd lockDesc

-- | Holds an exclusive write lock on the global lock file
-- for the duration of the given action.
withStoreFileLock :: ContentStore -> IO a -> IO a
withStoreFileLock store =
  bracket_ (acquireStoreFileLock store) (releaseStoreFileLock store)

-- | Holds a lock on the global 'MVar' and on the global lock file
-- for the duration of the given action.
withStoreLock :: ContentStore -> IO a -> IO a
withStoreLock store action =
  withMVar (storeLock store) $ \() ->
    withStoreFileLock store $
      action

-- | Return the full store path to the given hash.
storePath :: ContentStore -> ContentHash -> FilePath
storePath ContentStore {storeRoot} hash = storeRoot </> hashToPath hash

-- | Query the state of a subtree without taking a lock.
internalQuery :: ContentStore -> ContentHash -> IO (Status () () ())
internalQuery store hash =
  let dir = storePath store hash in
  doesDirectoryExist dir >>= \case
    False -> return $ Missing ()
    True -> isWritable dir >>= \case
      False -> return $ Complete ()
      True -> return $ Pending ()

-- | Watch the given subtree under construction.
-- The returned 'Async' completes after the subtree is completed or failed.
internalWatchPending
  :: ContentStore
  -> ContentHash
  -> IO (Async Update)
internalWatchPending store hash = do
  let dir = storePath store hash
  -- Add an inotify watch and give a signal on relevant events.
  let inotify = storeINotify store
      mask = [Attrib, MoveSelf, DeleteSelf, OnlyDir]
  signal <- newEmptyMVar
  -- Signal the listener. If the 'MVar' is full,
  -- the listener didn't handle earlier signals, yet.
  let giveSignal = void $ tryPutMVar signal ()
  watch <- addWatch inotify mask dir $ \case
    Attributes True Nothing -> giveSignal
    MovedSelf True -> giveSignal
    DeletedSelf -> giveSignal
    _ -> return ()
  -- Additionally, poll on regular intervals.
  -- Inotify doesn't cover all cases, e.g. network filesystems.
  let tenMinutes = 10 * 60 * 1000000
  ticker <- async $ forever $ threadDelay tenMinutes >> giveSignal
  let stopWatching = do
        cancel ticker
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
        removeWatch watch `catch` \(_::IOError) -> return ()
  -- Listen to the signal asynchronously,
  -- and query the status when it fires.
  -- If the status changed, fill in the update.
  update <- newEmptyMVar
  let loop = takeMVar signal >> query store hash >>= \case
        Pending () -> loop
        Complete () -> tryPutMVar update $ Completed (Item dir)
        Missing () -> tryPutMVar update Failed
  void $ async loop
  -- Wait for the update asynchronously.
  -- Stop watching when it arrives.
  async $ takeMVar update <* stopWatching

setRootDirWritable :: ContentStore -> IO ()
setRootDirWritable ContentStore {storeRoot} =
  setFileMode storeRoot writableRootDirMode

writableRootDirMode :: FileMode
writableRootDirMode = writableDirMode

setRootDirReadOnly :: ContentStore -> IO ()
setRootDirReadOnly ContentStore {storeRoot} =
  setFileMode storeRoot readOnlyRootDirMode

readOnlyRootDirMode :: FileMode
readOnlyRootDirMode = writableDirMode `intersectFileModes` allButWritableMode

withWritableStore :: ContentStore -> IO a -> IO a
withWritableStore store =
  bracket_ (setRootDirWritable store) (setRootDirReadOnly store)

isWritable :: FilePath -> IO Bool
isWritable fp = fileAccess fp False True False

setDirWritable :: FilePath -> IO ()
setDirWritable fp = setFileMode fp writableDirMode

writableDirMode :: FileMode
writableDirMode = foldl' unionFileModes nullFileMode
  [ directoryMode, ownerModes
  , groupReadMode, groupExecuteMode
  , otherReadMode, otherExecuteMode
  ]

-- | Unset write permissions on the given path.
unsetWritable :: FilePath -> IO ()
unsetWritable fp = do
  mode <- fileMode <$> getFileStatus fp
  setFileMode fp $ mode `intersectFileModes` allButWritableMode

allButWritableMode :: FileMode
allButWritableMode = complement $ foldl' unionFileModes nullFileMode
  [ownerWriteMode, groupWriteMode, otherWriteMode]

-- | Unset write permissions on all items in a directory tree recursively.
unsetWritableRecursively :: FilePath -> IO ()
unsetWritableRecursively = mapFSTree unsetWritable unsetWritable

-- | @mapFSTree fDir fFile fp@ visits every item under the path @fp@ recursively
-- (including @fp@) and applies @fDir@ to directory paths and @fFile@ to
-- file paths.
--
-- Assumes that the path exists, either as directory or as file.
-- Symlinks are treated as files.
mapFSTree :: (FilePath -> IO ()) -> (FilePath -> IO ()) -> FilePath -> IO ()
mapFSTree fDir fFile fp = doesDirectoryExist fp >>= \case
  False -> fFile fp
  True -> do
    fDir fp
    entries <- listDirectory fp
    let fps = map (fp </>) entries
    mapM_ (mapFSTree fDir fFile) fps
