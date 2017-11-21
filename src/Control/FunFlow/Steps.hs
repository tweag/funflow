{-# LANGUAGE Arrows              #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Control.FunFlow.Steps where

import           Control.Arrow
import           Control.Arrow.Free              (catch)
import           Control.Exception               (Exception)
import           Control.FunFlow.Base
import           Control.FunFlow.ContentHashable ( DirectoryContent (..)
                                                 , FileContent (..) )
import qualified Control.FunFlow.ContentStore    as CS
import           Control.Monad.Catch             (throwM)
import           Data.Store
import           GHC.Conc                        (threadDelay)
import           Path
import           Path.IO
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

-- | `retry n s f` reruns `f` on failure at most n times with a delay of `s`
--   seconds between retries
retry :: forall eff ex a b. (Exception ex, Store a)
      => Int -> Int -> Flow eff ex a b -> Flow eff ex a b
retry 0 _ f = f
retry n secs f = catch f $ proc (x, _ :: ex) -> do
  x1 <- pauseWith -< (secs,x)
  x2 <- retry (n-1) secs f -< x1
  returnA -< x2

-- | @copyFileToStore (fIn, fOut)@ copies the contents of @fIn@ into the store
-- under the relative path @fOut@ within the subtree.
copyFileToStore :: Flow eff ex (FileContent, Path Rel File) CS.Item
copyFileToStore = putInStore $ \d (FileContent inFP, outFP) -> do
  createDirIfMissing True (parent $ d </> outFP)
  copyFile inFP (d </> outFP)

-- | @copyDirToStore (dIn, Nothing)@ copies the contents of @dIn@ into the store
-- right under the subtree.
--
-- | @copyDirToStore (dIn, Just dOut)@ copies the contents of @dIn@ into the store
-- under relative path @dOut@ within the subtree
copyDirToStore :: Flow eff ex (DirectoryContent, Maybe (Path Rel Dir)) CS.Item
copyDirToStore = putInStore $ \d (DirectoryContent inDir, mbOutDir) ->
  case mbOutDir of
    Nothing -> copyDirRecur inDir d
    Just outDir -> do
      createDirIfMissing True (parent $ d </> outDir)
      copyDirRecur inDir (d </> outDir)

-- | Read the contents of the file named @out@ within the given subtree.
readOutFile :: Flow eff ex CS.Item String
readOutFile = getFromStore $ \d ->
  readFile (fromAbsFile $ d </> [relfile|out|])

-- | Create and write into a file named @out@ within the subtree.
writeOutFile :: Flow eff ex String CS.Item
writeOutFile = putInStore $ \d s ->
  writeFile (fromAbsFile $ d </> [relfile|out|]) s
