{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

-- | Miscallaneous steps to form part of Funflow computations.
module Control.Funflow.Steps
  ( -- * Error handling
    retry
    -- * Store manipulation
  , assignAliasInStore
  , copyDirToStore
  , copyFileToStore
  , listDirContents
  , globDir
  , globDirPattern
  , lookupAliasInStore
  , mergeDirs
  , mergeFiles
  , putInStoreAt
  , readString
  , readString_
  , readYaml
  , writeExecutableString
  , writeString
  , writeString_
  , writeYaml
  , writeYaml_
    -- * Docker
  , docker
    -- * Testing and debugging
  , promptFor
  , printS
  , failStep
  , cachedFailStep
  , worstBernoulli
  , pauseWith
  , melancholicLazarus
  )
where

import           Control.Arrow
import           Control.Arrow.Free              (catch)
import           Control.Exception.Safe          (Exception, throwM)
import           Control.Funflow.Base            (SimpleFlow, cache,
                                                  defaultCacherWithIdent)
import           Control.Funflow.Class
import qualified Control.Funflow.External.Docker as Docker
import           Data.CAS.ContentHashable        (ContentHashable,
                                                  DirectoryContent (..),
                                                  FileContent (..))
import           Data.CAS.ContentStore           (Content ((:</>)))
import qualified Data.CAS.ContentStore           as CS
import           Data.Default                    (def)
import           Data.Foldable                   (for_)
import           Data.Store
import           Data.Traversable                (for)
import           Data.Typeable                   (Typeable)
import qualified Data.Yaml                       as Yaml
import           GHC.Conc                        (threadDelay)
import           Path
import           Path.IO
import qualified System.FilePath.Glob            as Glob
import           System.Posix.Files              (accessModes, createLink,
                                                  setFileMode)
import           System.Random

promptFor :: (Read a, ArrowFlow eff ex arr) => arr String a
promptFor = proc s -> do
     () <- stepIO putStr -< (s++"> ")
     s' <- stepIO (const getLine) -< ()
     returnA -< read s'

printS :: (Show a, ArrowFlow eff ex arr) => arr a ()
printS = stepIO $ \s-> print s

failStep :: ArrowFlow eff ex arr => arr () ()
failStep = stepIO $ \_ -> fail "failStep"

cachedFailStep :: ArrowFlow eff ex arr => arr () ()
cachedFailStep = stepIO'
  (def { cache = defaultCacherWithIdent 1235314531893843918})
  (\_ -> fail "cachedFailStep")

worstBernoulli :: (Exception ex, ArrowFlow eff ex arr) => (String -> ex) -> arr Double Double
worstBernoulli errorC = stepIO $ \p -> do
  r <- randomRIO (0,1)
  if r < p
    then return r
    else throwM . errorC $ "worstBernoulli fail with "++ show r++ " > "++show p

-- | pause for a given number of seconds. Thread through a value to ensure
--   delay does not happen inparallel with other processing
pauseWith :: (Store a, ArrowFlow eff ex arr) => arr (Int, a) a
pauseWith = stepIO $ \(secs,a) -> do
  threadDelay (secs*1000000)
  return a

-- | on first invocation die and leave a suicide note
--   on second invocation it is resurrected and destroys suicide note, returning contents
melancholicLazarus :: ArrowFlow eff ex arr => arr String String
melancholicLazarus = stepIO $ \s -> do
  let fnm = [absfile|/tmp/lazarus_note|]
  ex <- doesFileExist fnm
  if ex
    then do s1 <- readFile (fromAbsFile fnm)
            removeFile fnm
            return s1
    else do writeFile (fromAbsFile fnm) s
            fail "lazarus fail"

-- | `retry n s f` reruns `f` on failure at most n times with a delay of `s`
--   seconds between retries
retry :: forall arr eff ex a b. (Exception ex, Store a, ArrowFlow eff ex arr, ArrowChoice arr)
      => Int -> Int -> arr a b -> arr a b
retry 0 _ f = f
retry n secs f = catch f $ proc (x, _ :: ex) -> do
  x1 <- pauseWith -< (secs,x)
  x2 <- retry (n-1) secs f -< x1
  returnA -< x2

lookupAliasInStore :: ArrowFlow eff ex arr => arr CS.Alias (Maybe CS.Item)
lookupAliasInStore = internalManipulateStore CS.lookupAlias
assignAliasInStore :: ArrowFlow eff ex arr => arr (CS.Alias, CS.Item) ()
assignAliasInStore = internalManipulateStore $ \store (alias, item) ->
  CS.assignAlias store alias item

putInStoreAt :: (ContentHashable IO a, Typeable t, ArrowFlow eff ex arr)
  => (Path Abs t -> a -> IO ()) -> arr (a, Path Rel t) (CS.Content t)
putInStoreAt f = proc (a, p) -> do
  item <- putInStore (\d (a, p) -> do
      createDirIfMissing True (parent $ d </> p)
      f (d </> p) a
    ) -< (a, p)
  returnA -< item :</> p

-- | @copyFileToStore (fIn, fOut)@ copies the contents of @fIn@ into the store
-- under the relative path @fOut@ within the subtree.
copyFileToStore :: ArrowFlow eff ex arr => arr (FileContent, Path Rel File) (CS.Content File)
copyFileToStore = putInStoreAt $ \p (FileContent inFP) -> copyFile inFP p

-- | @copyDirToStore (dIn, Nothing)@ copies the contents of @dIn@ into the store
-- right under the subtree.
--
-- | @copyDirToStore (dIn, Just dOut)@ copies the contents of @dIn@ into the store
-- under relative path @dOut@ within the subtree
copyDirToStore :: (ArrowChoice arr, ArrowFlow eff ex arr)
               => arr (DirectoryContent, Maybe (Path Rel Dir)) (CS.Content Dir)
copyDirToStore = proc (inDir, mbOutDir) ->
  case mbOutDir of
    Nothing -> do
      item <- putInStore (\d (DirectoryContent inDir) ->
          copyDirRecur inDir d
        ) -< inDir
      returnA -< CS.All item
    Just outDir ->
      putInStoreAt (\p (DirectoryContent inDir) ->
          copyDirRecur inDir p
        ) -< (inDir, outDir)

-- | List the contents of a directory within the store
listDirContents :: ArrowFlow eff ex arr => arr (CS.Content Dir)
                               ([CS.Content Dir], [CS.Content File])
listDirContents = internalManipulateStore
  ( \store dir -> let
        item = CS.contentItem dir
        itemRoot = CS.itemPath store item
      in do
        (dirs, files) <- listDir $ CS.contentPath store dir
        relDirs <- for dirs (stripProperPrefix itemRoot)
        relFiles <- for files (stripProperPrefix itemRoot)
        return ( (item :</>) <$> relDirs
               , (item :</>) <$> relFiles
               )
  )

-- | Search for files in the directory matching the given text string, as a glob
-- pattern.
globDir :: ArrowFlow eff ex arr
        => arr (CS.Content Dir, String) [CS.Content File]
globDir = globDirPattern <<< second (arr $ Glob.simplify . Glob.compile)

-- | Search for files in the directory matching the given pattern.
globDirPattern :: ArrowFlow eff ex arr
               => arr (CS.Content Dir, Glob.Pattern) [CS.Content File]
globDirPattern = internalManipulateStore
  ( \store (dir, patt) -> let
      item = CS.contentItem dir
      itemRoot = CS.itemPath store item
    in do
      files <- mapM parseAbsFile =<< Glob.globDir1 patt (toFilePath itemRoot)
      relFiles <- for files (stripProperPrefix itemRoot)
      return ( (item :</>) <$> relFiles )
  )

-- | Merge a number of store directories together into a single output directory.
--   This uses hardlinks to avoid duplicating the data on disk.
mergeDirs :: ArrowFlow eff ex arr => arr [CS.Content Dir] (CS.Content Dir)
mergeDirs = proc inDirs -> do
  paths <- internalManipulateStore
    ( \store items -> return $ CS.contentPath store <$> items) -< inDirs
  arr CS.All <<< putInStore
    ( \d inDirs -> for_ inDirs $ \inDir -> do
      (subDirs, files) <- listDirRecur inDir
      for_ subDirs $ \absSubDir -> do
        relSubDir <- stripProperPrefix inDir absSubDir
        createDirIfMissing True (d </> relSubDir)
      for_ files $ \absFile -> do
        relFile <- stripProperPrefix inDir absFile
        createLink (toFilePath absFile) (toFilePath $ d </> relFile)
    ) -< paths

-- | Merge a number of files into a single output directory.
mergeFiles :: ArrowFlow eff ex arr => arr [CS.Content File] (CS.Content Dir)
mergeFiles = proc inFiles -> do
  absFiles <- internalManipulateStore
    ( \store items -> return $ CS.contentPath store <$> items) -< inFiles
  arr CS.All <<< putInStore
    (\d inFiles -> for_ inFiles $ \inFile ->
      createLink (toFilePath inFile) (toFilePath $ d </> filename inFile)
    ) -< absFiles

-- | Read the contents of the given file in the store.
readString :: ArrowFlow eff ex arr => arr (CS.Content File) String
readString = getFromStore $ readFile . fromAbsFile

-- | Read the contents of a file named @out@ within the given item.
readString_ :: ArrowFlow eff ex arr => arr CS.Item String
readString_ = arr (:</> [relfile|out|]) >>> readString

-- | Create and write into a file under the given path in the store.
writeString :: ArrowFlow eff ex arr => arr (String, Path Rel File) (CS.Content File)
writeString = putInStoreAt $ writeFile . fromAbsFile

writeExecutableString :: ArrowFlow eff ex arr => arr (String, Path Rel File) (CS.Content File)
writeExecutableString = putInStoreAt $ \p i -> do
  writeFile (fromAbsFile p) i
  setFileMode (fromAbsFile p) accessModes

-- | Create and write into a file named @out@ within the given item.
writeString_ :: ArrowFlow eff ex arr => arr String (CS.Content File)
writeString_ = Control.Funflow.Steps.writeString <<< arr (, [relfile|out|])

-- | Read a YAML file from the given file in the store.
readYaml :: Yaml.FromJSON a
  => SimpleFlow (CS.Content File) (Either Yaml.ParseException a)
readYaml = getFromStore (Yaml.decodeFileEither . fromAbsFile)

-- | Write a YAML file under the given name to the store.
writeYaml :: (ContentHashable IO a, Yaml.ToJSON a)
  => SimpleFlow (a, Path Rel File) (CS.Content File)
writeYaml = putInStoreAt $ Yaml.encodeFile . fromAbsFile

-- | Write a YAML file named @out.yaml@ to the store.
writeYaml_ :: (ContentHashable IO a, Yaml.ToJSON a)
  => SimpleFlow a (CS.Content File)
writeYaml_ = writeYaml <<< arr (, [relfile|out.yaml|])

docker :: (ContentHashable IO a, ArrowFlow eff ex arr) => (a -> Docker.Config) -> arr a CS.Item
docker f = external $ Docker.toExternal . f
