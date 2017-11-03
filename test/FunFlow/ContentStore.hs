{-# LANGUAGE LambdaCase #-}

module FunFlow.ContentStore
  ( tests
  ) where

import           Control.Exception.Safe          (tryAny)
import           Control.FunFlow.ContentHashable (contentHash)
import           Control.FunFlow.ContentStore    (ContentStore)
import qualified Control.FunFlow.ContentStore    as ContentStore
import           Control.Monad                   (void)
import qualified Data.Set                        as Set
import           System.Directory
import           System.FilePath                 ((</>))
import           System.IO.Temp
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Content Store"

  [ testCase "initialize fresh store" $
    withTmpDir $ \dir -> do
      let root = dir </> "store"
      void $ ContentStore.initialize root
      doesDirectoryExist root
        @? "store root exists"

  , testCase "initialize existing store" $
    withTmpDir $ \dir -> do
      let root = dir </> "store"
      createDirectory root
      void $ ContentStore.initialize root
      doesDirectoryExist root
        @? "store root exists"

  , testCase "store is not writable" $
    withEmptyStore $ \store -> do
      let root = ContentStore.root store
      not . writable <$> getPermissions root
        @? "store not writable"
      writeFile (root </> "test") "Hello world"
        `shouldFail` "can't create file in store"

  , testCase "subtree stages" $
    withEmptyStore $ \store -> do
      hash <- contentHash "test"

      missing <- ContentStore.query store hash
      missing @?= ContentStore.Missing
      nothing <- ContentStore.lookup store hash
      nothing @?= Nothing

      subtree <- ContentStore.markUnderConstruction store hash
      let dir = subtree </> "dir"
          item = dir </> "file"
          expectedContent = "Hello World"
      underConstruction <- ContentStore.query store hash
      underConstruction @?= ContentStore.UnderConstruction
      nothing' <- ContentStore.lookup store hash
      nothing' @?= Nothing
      doesDirectoryExist subtree
        @? "subtree exists"
      writable <$> getPermissions subtree
        @? "subtree is writable"
      createDirectory dir
      writeFile item expectedContent
      do
        content <- readFile item
        content @?= expectedContent

      ContentStore.markComplete store hash
      complete <- ContentStore.query store hash
      complete @?= ContentStore.Complete
      justSubtree <- ContentStore.lookup store hash
      justSubtree @?= Just subtree
      doesDirectoryExist subtree
        @? "subtree exists"
      not . writable <$> getPermissions subtree
        @? "subtree is not writable"
      not . writable <$> getPermissions item
        @? "item is not writable"
      createDirectory (subtree </> "another")
        `shouldFail` "can't create folder in complete subtree"
      writeFile item "Another message"
        `shouldFail` "can't write to complete item"
      do
        content <- readFile item
        content @?= expectedContent

  , testCase "construct if missing" $
    withEmptyStore $ \store -> do
      hash <- contentHash "test"
      let file = "file"
          expectedContent = "Hello World"

      ContentStore.constructIfMissing store hash >>= \case
        ContentStore.Wait ->
          assertFailure "missing already under construction"
        ContentStore.Consume _ ->
          assertFailure "missing already complete"
        ContentStore.Construct subtree -> do
          writable <$> getPermissions subtree
            @? "under construction not writable"
          writeFile (subtree </> file) expectedContent

      ContentStore.constructIfMissing store hash >>= \case
        ContentStore.Construct _ ->
          assertFailure "under construction still missing"
        ContentStore.Consume _ ->
          assertFailure "under construction already complete"
        ContentStore.Wait ->
          ContentStore.markComplete store hash

      ContentStore.constructIfMissing store hash >>= \case
        ContentStore.Construct _ ->
          assertFailure "complete still missing"
        ContentStore.Wait ->
          assertFailure "complete still under construction"
        ContentStore.Consume subtree -> do
          not . writable <$> getPermissions (subtree </> file)
            @? "complete still writable"
          content <- readFile (subtree </> file)
          content @?= expectedContent

  , testCase "remove failed" $
    withEmptyStore $ \store -> do
      hash <- contentHash "test"
      subtree <- ContentStore.markUnderConstruction store hash
      ContentStore.removeFailed store hash
      not <$> doesDirectoryExist subtree
        @? "subtree was removed"

  , testCase "forcibly remove" $
    withEmptyStore $ \store -> do
      hash <- contentHash "test"
      subtree <- ContentStore.markUnderConstruction store hash

      ContentStore.removeForcibly store hash
      not <$> doesDirectoryExist subtree
        @? "remove under construction"

      ContentStore.removeForcibly store hash
      not <$> doesDirectoryExist subtree
        @? "remove missing"

      subtree' <- ContentStore.markUnderConstruction store hash
      ContentStore.markComplete store hash
      ContentStore.removeForcibly store hash
      not <$> doesDirectoryExist subtree'
        @? "remove complete"

  , testCase "subtree state is persisted" $
    withTmpDir $ \dir -> do
      let root = dir </> "store"
      hash <- contentHash "test"

      do
        store <- ContentStore.initialize root
        void $ ContentStore.markUnderConstruction store hash

      -- Imagine the process terminates and the store is closed

      do
        store <- ContentStore.initialize root
        underConstruction <- ContentStore.query store hash
        underConstruction @?= ContentStore.UnderConstruction
        ContentStore.markComplete store hash

      -- Imagine the process terminates and the store is closed

      do
        store <- ContentStore.initialize root
        complete <- ContentStore.query store hash
        complete @?= ContentStore.Complete

  , testCase "mark complete before under construction fails" $
    withEmptyStore $ \store -> do
      hash <- contentHash "test"
      ContentStore.markComplete store hash
        `shouldFail` "complete before under construction"

  , testCase "mark complete after complete fails" $
    withEmptyStore $ \store -> do
      hash <- contentHash "test"
      void $ ContentStore.markUnderConstruction store hash
      ContentStore.markComplete store hash
      ContentStore.markComplete store hash
        `shouldFail` "complete after complete"

  , testCase "mark under construction after under construction fails" $
    withEmptyStore $ \store -> do
      hash <- contentHash "test"
      void $ ContentStore.markUnderConstruction store hash
      void $ ContentStore.markUnderConstruction store hash
        `shouldFail` "under construction after under construction"

  , testCase "mark under construction after complete fails" $
    withEmptyStore $ \store -> do
      hash <- contentHash "test"
      void $ ContentStore.markUnderConstruction store hash
      ContentStore.markComplete store hash
      void $ ContentStore.markUnderConstruction store hash
        `shouldFail` "under construction after complete"

  , testCase "remove missing fails" $
    withEmptyStore $ \store -> do
      hash <- contentHash "test"
      ContentStore.removeFailed store hash
        `shouldFail` "remove non existent"

  , testCase "remove complete fails" $
    withEmptyStore $ \store -> do
      hash <- contentHash "test"
      void $ ContentStore.markUnderConstruction store hash
      ContentStore.markComplete store hash
      ContentStore.removeFailed store hash
        `shouldFail` "remove complete"

  , testCase "list store contents" $
    withEmptyStore $ \store -> do
      [a, b, c, d] <- mapM contentHash ["a", "b", "c", "d"]
      void $ mapM (ContentStore.markUnderConstruction store) [a, b, c, d]
      mapM_ (ContentStore.markComplete store) [a, b]

      all' <- ContentStore.allSubtrees store
      Set.fromList all' @?= Set.fromList [a, b, c, d]
      Set.size (Set.fromList all') @?= 4

      complete <- ContentStore.subtrees store
      Set.fromList complete @?= Set.fromList [a, b]

      underContsruction <- ContentStore.subtreesUnderConstruction store
      Set.fromList underContsruction @?= Set.fromList [c, d]

  ]

shouldFail :: IO a -> String -> IO ()
shouldFail m msg = tryAny m >>= \case
  Left _ -> return ()
  Right _ -> assertFailure msg

withTmpDir :: (FilePath -> IO a) -> IO a
withTmpDir = withSystemTempDirectory "funflow-teset"

withEmptyStore :: (ContentStore -> IO a) -> IO a
withEmptyStore k = withTmpDir $ \dir ->
  ContentStore.initialize (dir </> "store") >>= k
