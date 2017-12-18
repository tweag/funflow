{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

module Control.FunFlow.Steps
  ( -- * Error handling
    retry
    -- * Store manipulation
  , putInStoreAt
  , copyFileToStore
  , copyDirToStore
  , listDirContents
  , mergeDirs
  , mergeFiles
  , readString
  , readString_
  , writeString
  , writeString_
  , readYaml
  , writeYaml
  , writeYaml_
    -- * Testing and debugging
  , promptFor
  , printS
  , failStep
  , worstBernoulli
  , pauseWith
  , melancholicLazarus
  )
where

import           Control.Arrow
import           Control.Arrow.Free              (catch, effect)
import           Control.Exception               (Exception)
import           Control.FunFlow.Base
import           Control.FunFlow.ContentHashable (ContentHashable,
                                                  DirectoryContent (..),
                                                  FileContent (..))
import           Control.FunFlow.ContentStore    (Content ((:</>)))
import qualified Control.FunFlow.ContentStore    as CS
import           Control.Monad.Catch             (throwM)
import           Data.Foldable                   (for_)
import           Data.Store
import           Data.Traversable                (for)
import           Data.Typeable                   (Typeable)
import qualified Data.Yaml                       as Yaml
import           GHC.Conc                        (threadDelay)
import           Path
import           Path.IO
import           System.Posix.Files              (createLink)
import           System.Random

promptFor :: Read a => Flow eff ex String a
promptFor = proc s -> do
     () <- stepIO putStr -< (s++"> ")
     s' <- stepIO (const getLine) -< ()
     returnA -< read s'

printS :: Show a => Flow eff ex a ()
printS = stepIO $ \s-> print s

failStep :: Flow eff ex () ()
failStep = stepIO $ \_ -> fail "failStep"

worstBernoulli :: Exception ex => (String -> ex) -> Flow eff ex Double Double
worstBernoulli errorC = stepIO $ \p -> do
  r <- randomRIO (0,1)
  if r < p
    then return r
    else throwM . errorC $ "worstBernoulli fail with "++ show r++ " > "++show p

-- | pause for a given number of seconds. Thread through a value to ensure
--   delay does not happen inparallel with other processing
pauseWith :: Store a => Flow eff ex (Int, a) a
pauseWith = stepIO $ \(secs,a) -> do
  threadDelay (secs*1000000)
  return a

-- | on first invocation die and leave a suicide note
--   on second invocation it is resurrected and destroys suicide note, returning contents
melancholicLazarus :: Flow eff ex String String
melancholicLazarus = stepIO $ \s -> do
  let fnm = [absfile|/tmp/lazarus_note|]
  ex <- doesFileExist fnm
  if ex
    then do s1 <- readFile (fromAbsFile fnm)
            removeFile fnm
            return s1
    else do writeFile (fromAbsFile fnm) s
            fail "lazarus fail"

internalManipulateStore :: (CS.ContentStore -> a -> IO b) -> Flow eff ex a b
internalManipulateStore = effect . InternalManipulateStore

-- | `retry n s f` reruns `f` on failure at most n times with a delay of `s`
--   seconds between retries
retry :: forall eff ex a b. (Exception ex, Store a)
      => Int -> Int -> Flow eff ex a b -> Flow eff ex a b
retry 0 _ f = f
retry n secs f = catch f $ proc (x, _ :: ex) -> do
  x1 <- pauseWith -< (secs,x)
  x2 <- retry (n-1) secs f -< x1
  returnA -< x2

putInStoreAt :: (ContentHashable IO a, Typeable t)
  => (Path Abs t -> a -> IO ()) -> Flow eff ex (a, Path Rel t) (CS.Content t)
putInStoreAt f = proc (a, p) -> do
  item <- putInStore (\d (a, p) -> do
      createDirIfMissing True (parent $ d </> p)
      f (d </> p) a
    ) -< (a, p)
  returnA -< item :</> p

-- | @copyFileToStore (fIn, fOut)@ copies the contents of @fIn@ into the store
-- under the relative path @fOut@ within the subtree.
copyFileToStore :: Flow eff ex (FileContent, Path Rel File) (CS.Content File)
copyFileToStore = putInStoreAt $ \p (FileContent inFP) -> copyFile inFP p

-- | @copyDirToStore (dIn, Nothing)@ copies the contents of @dIn@ into the store
-- right under the subtree.
--
-- | @copyDirToStore (dIn, Just dOut)@ copies the contents of @dIn@ into the store
-- under relative path @dOut@ within the subtree
copyDirToStore :: Flow eff ex (DirectoryContent, Maybe (Path Rel Dir)) (CS.Content Dir)
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
listDirContents :: Flow eff ex (CS.Content Dir)
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

-- | Merge a number of store directories together into a single output directory.
--   This uses hardlinks to avoid duplicating the data on disk.
mergeDirs :: Flow eff ex [CS.Content Dir] (CS.Content Dir)
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
mergeFiles :: Flow eff ex [CS.Content File] (CS.Content Dir)
mergeFiles = mergeDirs <<< arr (fmap containingDir)
  where
    containingDir :: CS.Content File -> CS.Content Dir
    containingDir (item :</> dir) = item :</> parent dir

-- | Read the contents of the given file in the store.
readString :: Flow eff ex (CS.Content File) String
readString = getFromStore $ readFile . fromAbsFile

-- | Read the contents of a file named @out@ within the given item.
readString_ :: Flow eff ex CS.Item String
readString_ = arr (:</> [relfile|out|]) >>> readString

-- | Create and write into a file under the given path in the store.
writeString :: Flow eff ex (String, Path Rel File) (CS.Content File)
writeString = putInStoreAt $ writeFile . fromAbsFile

-- | Create and write into a file named @out@ within the given item.
writeString_ :: Flow eff ex String (CS.Content File)
writeString_ = Control.FunFlow.Steps.writeString <<< arr (, [relfile|out|])

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
