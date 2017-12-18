{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module FunFlow.ContentStore
  ( tests
  ) where

import           Control.Concurrent.Async
import           Control.Exception.Safe          (tryAny)
import           Control.FunFlow.ContentHashable (contentHash)
import           Control.FunFlow.ContentStore    (ContentStore)
import qualified Control.FunFlow.ContentStore    as ContentStore
import           Control.Monad                   (void)
import           Data.Maybe                      (catMaybes)
import qualified Data.Set                        as Set
import           Numeric
import           Path
import           Path.IO
import           System.Posix.Files
import           System.Posix.User
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Content Store"

  [ testCase "initialise fresh store" $
    withTmpDir $ \dir -> do
      let root = dir </> [reldir|store|]
      ContentStore.withStore root $ \_ ->
        doesDirExist @IO root
          @? "store root exists"

  , testCase "initialise existing store" $
    withTmpDir $ \dir -> do
      let root = dir </> [reldir|store|]
      createDir root
      ContentStore.withStore root $ \_ ->
        doesDirExist @IO root
          @? "store root exists"

  , testCase "store is not writable" $
    withEmptyStore $ \store -> do
      let root = ContentStore.root store
      do
        perms <- getPermissions @IO root
        putStrLn ""
        putStrLn $ "getPermissions: " ++ show perms
        fm <- fileMode <$> getFileStatus (fromAbsDir root)
        putStrLn $ "fileMode/getFileStatus: " ++ showOct fm ""
        ruid <- getRealUserID
        euid <- getEffectiveUserID
        putStrLn $ "real UID: " ++ show ruid ++ "  eff UID: " ++ show euid
      not . writable <$> getPermissions @IO root
        @? "store not writable"
      writeFile (fromAbsFile $ root </> [relfile|test|]) "Hello world"
        `shouldFail` "can't create file in store"

  , testCase "subtree stages" $
    withEmptyStore $ \store -> do
      hash <- contentHash ("test" :: String)

      missing <- ContentStore.query store hash
      missing @?= ContentStore.Missing ()
      missing' <- ContentStore.lookup store hash
      missing' @?= ContentStore.Missing ()

      subtree <- ContentStore.markPending store hash
      let dir = subtree </> [reldir|dir|]
          file = dir </> [relfile|file|]
          expectedContent = "Hello World"
      pending <- ContentStore.query store hash
      pending @?= ContentStore.Pending ()
      pending' <- ContentStore.lookup store hash
      pending' @?= ContentStore.Pending ()
      doesDirExist @IO subtree
        @? "pending subtree exists"
      writable <$> getPermissions @IO subtree
        @? "pending subtree is writable"
      createDir dir
      writeFile (fromAbsFile file) expectedContent
      do
        content <- readFile (fromAbsFile file)
        content @?= expectedContent

      item <- ContentStore.markComplete store hash
      let itemDir = ContentStore.itemPath store item
          file' = itemDir </> [relfile|dir/file|]
      complete <- ContentStore.query store hash
      complete @?= ContentStore.Complete ()
      complete' <- ContentStore.lookup store hash
      complete' @?= ContentStore.Complete item
      doesDirExist @IO itemDir
        @? "complete subtree exists"

      do
        perms <- getPermissions @IO itemDir
        putStrLn ""
        putStrLn $ "getPermissions: " ++ show perms
        fm <- fileMode <$> getFileStatus (fromAbsDir itemDir)
        putStrLn $ "fileMode/getFileStatus: " ++ showOct fm ""
        ruid <- getRealUserID
        euid <- getEffectiveUserID
        putStrLn $ "real UID: " ++ show ruid ++ "  eff UID: " ++ show euid
      not . writable <$> getPermissions @IO itemDir
        @? "complete subtree is not writable"
      not . writable <$> getPermissions @IO file'
        @? "complete file is not writable"
      createDir (itemDir </> [reldir|another|])
        `shouldFail` "can't create folder in complete subtree"
      writeFile (fromAbsFile file') "Another message"
        `shouldFail` "can't write to complete file"
      do
        content <- readFile (fromAbsFile file')
        content @?= expectedContent

  , testCase "await construction" $
    withEmptyStore $ \store -> do
      hash <- contentHash ("test" :: String)

      ContentStore.constructOrWait store hash >>= \case
        ContentStore.Pending _ ->
          assertFailure "missing already under construction"
        ContentStore.Complete _ ->
          assertFailure "missing already complete"
        ContentStore.Missing _ ->
          return ()

      a <- ContentStore.constructOrWait store hash >>= \case
        ContentStore.Missing _ -> do
          assertFailure "under construction still missing"
          undefined
        ContentStore.Complete _ -> do
          assertFailure "under construction already complete"
          undefined
        ContentStore.Pending a ->
          return a

      b <- ContentStore.lookupOrWait store hash >>= \case
        ContentStore.Missing _ -> do
          assertFailure "under construction still missing"
          undefined
        ContentStore.Complete _ -> do
          assertFailure "under construction already complete"
          undefined
        ContentStore.Pending b ->
          return b

      item <- ContentStore.markComplete store hash

      item' <- wait a
      item' @?= ContentStore.Completed item

      item'' <- wait b
      item'' @?= ContentStore.Completed item

      ContentStore.constructOrWait store hash >>= \case
        ContentStore.Missing _ -> do
          assertFailure "complete still missing"
        ContentStore.Pending _ -> do
          assertFailure "complete still under construction"
        ContentStore.Complete _ -> do
          return ()

  , testCase "await failure" $
    withEmptyStore $ \store -> do
      hash <- contentHash ("test" :: String)

      ContentStore.constructOrWait store hash >>= \case
        ContentStore.Pending _ ->
          assertFailure "missing already under construction"
        ContentStore.Complete _ ->
          assertFailure "missing already complete"
        ContentStore.Missing _ ->
          return ()

      a <- ContentStore.constructOrWait store hash >>= \case
        ContentStore.Missing _ -> do
          assertFailure "under construction still missing"
          undefined
        ContentStore.Complete _ -> do
          assertFailure "under construction already complete"
          undefined
        ContentStore.Pending a ->
          return a

      b <- ContentStore.lookupOrWait store hash >>= \case
        ContentStore.Missing _ -> do
          assertFailure "under construction still missing"
          undefined
        ContentStore.Complete _ -> do
          assertFailure "under construction already complete"
          undefined
        ContentStore.Pending b ->
          return b

      ContentStore.removeFailed store hash

      item' <- wait a
      item' @?= ContentStore.Failed

      item'' <- wait b
      item'' @?= ContentStore.Failed

      ContentStore.constructOrWait store hash >>= \case
        ContentStore.Pending _ -> do
          assertFailure "failed still under construction"
        ContentStore.Complete _ -> do
          assertFailure "failed already complete"
        ContentStore.Missing _ -> do
          return ()

  , testCase "construct if missing" $
    withEmptyStore $ \store -> do
      hash <- contentHash ("test" :: String)
      let file = [relfile|file|]
          expectedContent = "Hello World"

      ContentStore.constructIfMissing store hash >>= \case
        ContentStore.Pending () ->
          assertFailure "missing already under construction"
        ContentStore.Complete _ ->
          assertFailure "missing already complete"
        ContentStore.Missing subtree -> do
          writable <$> getPermissions @IO subtree
            @? "under construction not writable"
          writeFile (fromAbsFile $ subtree </> file) expectedContent

      ContentStore.constructIfMissing store hash >>= \case
        ContentStore.Missing _ ->
          assertFailure "under construction still missing"
        ContentStore.Complete _ ->
          assertFailure "under construction already complete"
        ContentStore.Pending () ->
          void $ ContentStore.markComplete store hash

      ContentStore.constructIfMissing store hash >>= \case
        ContentStore.Missing _ ->
          assertFailure "complete still missing"
        ContentStore.Pending () ->
          assertFailure "complete still under construction"
        ContentStore.Complete item -> do
          let subtree = ContentStore.itemPath store item
          do
            perms <- getPermissions @IO (subtree </> file)
            putStrLn ""
            putStrLn $ "getPermissions: " ++ show perms
            fm <- fileMode <$> getFileStatus (fromAbsFile $ subtree </> file)
            putStrLn $ "fileMode/getFileStatus: " ++ showOct fm ""
            ruid <- getRealUserID
            euid <- getEffectiveUserID
            putStrLn $ "real UID: " ++ show ruid ++ "  eff UID: " ++ show euid
          not . writable <$> getPermissions @IO (subtree </> file)
            @? "complete still writable"
          content <- readFile (fromAbsFile $ subtree </> file)
          content @?= expectedContent

  , testCase "remove failed" $
    withEmptyStore $ \store -> do
      hash <- contentHash ("test" :: String)
      subtree <- ContentStore.markPending store hash
      ContentStore.removeFailed store hash
      not <$> doesDirExist @IO subtree
        @? "subtree was removed"

  , testCase "forcibly remove" $
    withEmptyStore $ \store -> do
      hash <- contentHash ("test" :: String)
      subtree <- ContentStore.markPending store hash

      ContentStore.removeForcibly store hash
      not <$> doesDirExist @IO subtree
        @? "remove under construction"

      ContentStore.removeForcibly store hash
      not <$> doesDirExist @IO subtree
        @? "remove missing"

      subtree' <- ContentStore.markPending store hash
      void $ ContentStore.markComplete store hash
      ContentStore.removeForcibly store hash
      not <$> doesDirExist @IO subtree'
        @? "remove complete"

  , testCase "subtree state is persisted" $
    withTmpDir $ \dir -> do
      let root = dir </> [reldir|store|]
      hash <- contentHash ("test" :: String)

      do
        ContentStore.withStore root $ \store ->
          void $ ContentStore.markPending store hash

      -- Imagine the process terminates and the store is closed

      do
        ContentStore.withStore root $ \store -> do
          underConstruction <- ContentStore.query store hash
          underConstruction @?= ContentStore.Pending ()
          void $ ContentStore.markComplete store hash

      -- Imagine the process terminates and the store is closed

      do
        ContentStore.withStore root $ \store -> do
          complete <- ContentStore.query store hash
          complete @?= ContentStore.Complete ()

  , testCase "mark complete before under construction fails" $
    withEmptyStore $ \store -> do
      hash <- contentHash ("test" :: String)
      ContentStore.markComplete store hash
        `shouldFail` "complete before under construction"

  , testCase "mark complete after complete fails" $
    withEmptyStore $ \store -> do
      hash <- contentHash ("test" :: String)
      void $ ContentStore.markPending store hash
      void $ ContentStore.markComplete store hash
      ContentStore.markComplete store hash
        `shouldFail` "complete after complete"

  , testCase "mark under construction after under construction fails" $
    withEmptyStore $ \store -> do
      hash <- contentHash ("test" :: String)
      void $ ContentStore.markPending store hash
      void $ ContentStore.markPending store hash
        `shouldFail` "under construction after under construction"

  , testCase "mark under construction after complete fails" $
    withEmptyStore $ \store -> do
      hash <- contentHash ("test" :: String)
      void $ ContentStore.markPending store hash
      void $ ContentStore.markComplete store hash
      void $ ContentStore.markPending store hash
        `shouldFail` "under construction after complete"

  , testCase "remove missing fails" $
    withEmptyStore $ \store -> do
      hash <- contentHash ("test" :: String)
      ContentStore.removeFailed store hash
        `shouldFail` "remove non existent"

  , testCase "remove complete fails" $
    withEmptyStore $ \store -> do
      hash <- contentHash ("test" :: String)
      void $ ContentStore.markPending store hash
      void $ ContentStore.markComplete store hash
      ContentStore.removeFailed store hash
        `shouldFail` "remove complete"

  , testCase "list store contents" $
    withEmptyStore $ \store -> do
      [a, b, c, d] <- mapM contentHash ["a", "b", "c", "d" :: String]
      void $ mapM (ContentStore.markPending store) [a, b, c, d]
      mapM_ (ContentStore.markComplete store) [a, b]

      (pendings, completes, items) <- ContentStore.listAll store
      let all' = pendings ++ completes
      Set.fromList all' @?= Set.fromList [a, b, c, d]
      Set.size (Set.fromList all') @?= 4

      underContsruction <- ContentStore.listPending store
      Set.fromList underContsruction @?= Set.fromList [c, d]

      complete <- ContentStore.listComplete store
      Set.fromList complete @?= Set.fromList [a, b]

      items' <- catMaybes <$> mapM (ContentStore.waitUntilComplete store) [a, b]
      Set.fromList items @?= Set.fromList items'

  , testCase "store aliases" $
    withEmptyStore $ \store -> do
      let fp = [relfile|test|]
          contentA = "itemA"
          contentB = "itemB"
      itemA <- do
        hash <- contentHash ("a" :: String)
        dir <- ContentStore.markPending store hash
        writeFile (fromAbsFile $ dir </> fp) contentA
        ContentStore.markComplete store hash
      itemB <- do
        hash <- contentHash ("b" :: String)
        dir <- ContentStore.markPending store hash
        writeFile (fromAbsFile $ dir </> fp) contentB
        ContentStore.markComplete store hash
      let aliasA = ContentStore.Alias "aliasA"
          aliasB = ContentStore.Alias "aliasB"

      do
        r <- ContentStore.lookupAlias store aliasA
        r @?= Nothing

      ContentStore.assignAlias store aliasA itemA
      ContentStore.assignAlias store aliasB itemB
      do
        r <- ContentStore.lookupAlias store aliasA
        r @?= Just itemA
      do
        r <- ContentStore.lookupAlias store aliasB
        r @?= Just itemB

      ContentStore.assignAlias store aliasB itemA
      do
        r <- ContentStore.lookupAlias store aliasA
        r @?= Just itemA
      do
        r <- ContentStore.lookupAlias store aliasB
        r @?= Just itemA

      ContentStore.removeAlias store aliasA
      ContentStore.removeAlias store aliasB
      do
        r <- ContentStore.lookupAlias store aliasA
        r @?= Nothing
      do
        r <- ContentStore.lookupAlias store aliasB
        r @?= Nothing

  ]

shouldFail :: IO a -> String -> IO ()
shouldFail m msg = tryAny m >>= \case
  Left _ -> return ()
  Right _ -> assertFailure msg

withTmpDir :: (Path Abs Dir -> IO a) -> IO a
withTmpDir = withSystemTempDir "funflow-test"

withEmptyStore :: (ContentStore -> IO a) -> IO a
withEmptyStore k = withTmpDir $ \dir ->
  ContentStore.withStore (dir </> [reldir|store|]) k
