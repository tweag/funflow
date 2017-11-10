{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}


-- | Hash addressed store in file system.
--
-- A store associates a 'Control.FunFlow.ContentHashable.ContentHash'
-- with a directory subtree. A subtree can be either
-- 'Control.FunFlow.ContentStore.Missing',
-- 'Control.FunFlow.ContentStore.UnderConstruction', or
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
-- subtree is marked as under construction.
--
-- __Implementation note:__
--
-- Two file-system features are used to persist the state of a subtree,
-- namely whether it exists and whether it is writable.
--
-- @
--   exists   writable          state
--   ---------------------------------------
--                             missing
--     X          X       under construction
--     X                       complete
-- @
module Control.FunFlow.ContentStore
  ( Status (..)
  , Instruction (..)
  , StoreError (..)
  , ContentStore
  , Item
  , itemPath
  , root
  , initialize
  , allSubtrees
  , subtrees
  , subtreesUnderConstruction
  , query
  , isMissing
  , isUnderConstruction
  , isComplete
  , lookup
  , constructIfMissing
  , markUnderConstruction
  , markComplete
  , removeFailed
  , removeForcibly
  ) where


import           Prelude                         hiding (lookup)

import           Control.Concurrent.MVar
import           Control.Exception               (Exception, bracket_, throwIO)
import           Control.Monad                   (filterM)
import           Data.Bits                       (complement)
import           Data.List                       (foldl')
import           Data.Maybe                      (catMaybes)
import qualified Data.Store
import           Data.Typeable                   (Typeable)
import           GHC.IO.Device                   (SeekMode (AbsoluteSeek))
import           GHC.Generics                    (Generic)
import           System.Directory                (createDirectory,
                                                  createDirectoryIfMissing,
                                                  doesDirectoryExist,
                                                  listDirectory, makeAbsolute,
                                                  removePathForcibly)
import           System.FilePath                 ((</>))
import           System.Posix.Files
import           System.Posix.IO
import           System.Posix.Types

import           Control.FunFlow.ContentHashable (ContentHash,
                                                  ContentHashable (..),
                                                  DirectoryContent (..),
                                                  hashToPath, pathToHash)


-- | Status of a subtree in the store.
data Status
  = Missing
  -- ^ The subtree does not exist, yet.
  | UnderConstruction
  -- ^ The subtree is under construction and not ready for consumption.
  | Complete
  -- ^ The subtree is complete and ready for consumption.
  deriving (Eq, Show)

-- | Instruction to the caller on what to do after calling
-- 'Control.FunFlow.ContentStore.constructIfMissing'.
data Instruction
  = Construct FilePath
  -- ^ The subtree was previously missing
  -- and is now marked as under construction.
  | Wait
  -- ^ The subtree is already under construction.
  | Consume Item
  -- ^ The subtree is already complete and ready for consumption.
  deriving (Eq, Show)

-- | Errors that can occur when interacting with the store.
data StoreError
  = NotUnderConstruction ContentHash
  -- ^ A subtree is not under construction when it should be.
  | AlreadyUnderConstruction ContentHash
  -- ^ A subtree is already under construction when it should be missing.
  | AlreadyComplete ContentHash
  -- ^ A subtree is already complete when it shouldn't be.
  deriving (Show, Typeable)
instance Exception StoreError

-- | A hash addressed store on the file system.
data ContentStore = ContentStore
  { storeRoot :: FilePath
  -- ^ Subtrees are stored directly under this directory.
  , storeLock :: MVar ()
  -- ^ One global lock on store metadata to ensure thread safety.
  -- The lock is taken when subtree state is changed or queried.
  , storeLockFd :: Fd
  -- ^ One exclusive file lock to ensure multi-processing safety.
  -- Note, that file locks are shared between threads in a process,
  -- so that the file lock needs to be complemented by an `MVar`
  -- for thread-safety.
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

-- | @initialize root@ initializes a store under the given root directory.
--
-- The root directory is created if necessary.
--
-- It is not safe to have multiple store objects
-- refer to the same root directory.
initialize :: FilePath -> IO ContentStore
initialize root' = do
  storeRoot <- makeAbsolute root'
  createDirectoryIfMissing True storeRoot
  storeLockFd <- createFile (lockPath storeRoot) ownerWriteMode
  setFileMode storeRoot readOnlyRootDirMode
  storeLock <- newMVar ()
  return ContentStore {..}

-- | List all subtrees that are complete or under construction.
allSubtrees :: ContentStore -> IO [ContentHash]
allSubtrees ContentStore {storeRoot} =
  catMaybes . map pathToHash <$> listDirectory storeRoot

-- | List all complete subtrees.
subtrees :: ContentStore -> IO [ContentHash]
subtrees store = filterM (isComplete store) =<< allSubtrees store

-- | List all subtrees under construction.
subtreesUnderConstruction :: ContentStore -> IO [ContentHash]
subtreesUnderConstruction store =
  filterM (isUnderConstruction store) =<< allSubtrees store

-- | Query for the state of a subtree.
query :: ContentStore -> ContentHash -> IO Status
query store hash = withStoreLock store $
  internalQuery store hash

isMissing :: ContentStore -> ContentHash -> IO Bool
isMissing store hash = (== Missing) <$> query store hash

isUnderConstruction :: ContentStore -> ContentHash -> IO Bool
isUnderConstruction store hash = (== UnderConstruction) <$> query store hash

isComplete :: ContentStore -> ContentHash -> IO Bool
isComplete store hash = (== Complete) <$> query store hash

-- | Get the file-path of a subtree if it is complete.
lookup :: ContentStore -> ContentHash -> IO (Maybe Item)
lookup store hash = withStoreLock store $
  internalQuery store hash >>= \case
    Missing -> return Nothing
    UnderConstruction -> return Nothing
    Complete -> return $ Just (Item (storePath store hash))

-- | Atomically query the state of a subtree
-- and mark it as under construction if missing.
constructIfMissing :: ContentStore -> ContentHash -> IO Instruction
constructIfMissing store hash = withStoreLock store $
  let dir = storePath store hash in
  internalQuery store hash >>= \case
    Complete -> return $ Consume (Item dir)
    UnderConstruction -> return Wait
    Missing -> withWritableStore store $ do
      createDirectory dir
      setDirWritable dir
      return $ Construct dir

-- | Mark a non-existent subtree as under construction.
--
-- Creates the destination directory and returns its path.
--
-- See also: 'Control.FunFlow.ContentStore.constructIfMissing'.
markUnderConstruction :: ContentStore -> ContentHash -> IO FilePath
markUnderConstruction store hash = withStoreLock store $
  internalQuery store hash >>= \case
    Complete -> throwIO (AlreadyComplete hash)
    UnderConstruction -> throwIO (AlreadyUnderConstruction hash)
    Missing -> withWritableStore store $ do
      let dir = storePath store hash
      createDirectory dir
      setDirWritable dir
      return dir

-- | Mark a subtree that was under construction as complete.
markComplete :: ContentStore -> ContentHash -> IO Item
markComplete store hash = withStoreLock store $
  internalQuery store hash >>= \case
    Missing -> throwIO (NotUnderConstruction hash)
    Complete -> throwIO (AlreadyComplete hash)
    UnderConstruction -> withWritableStore store $ do
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
    Missing -> throwIO (NotUnderConstruction hash)
    Complete -> throwIO (AlreadyComplete hash)
    UnderConstruction -> withWritableStore store $
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
internalQuery :: ContentStore -> ContentHash -> IO Status
internalQuery store hash =
  let dir = storePath store hash in
  doesDirectoryExist dir >>= \case
    False -> return Missing
    True -> isWritable dir >>= \case
      False -> return Complete
      True -> return UnderConstruction

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
