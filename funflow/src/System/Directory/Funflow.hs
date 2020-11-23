{-# LANGUAGE TypeApplications #-}

module System.Directory.Funflow (moveDirectoryContent) where

import Control.Exception (catch, throw)
import Control.Monad (filterM)
import Data.Maybe (catMaybes)
import Foreign.C.Error (Errno (Errno), eXDEV)
import GHC.IO.Exception (IOException (ioe_errno))
import Path (Abs, Dir, Path, dirname, filename, parseRelDir, parseRelFile, toFilePath, (</>))
import System.Directory (copyFile, doesDirectoryExist, doesFileExist, listDirectory, removeFile, renamePath)

-- | Move all the directories and files from a source directory to a target directory
moveDirectoryContent :: Path Abs Dir -> Path Abs Dir -> IO ()
moveDirectoryContent sourceDirectory targetDirectory =
  do
    -- List of directories inside
    dirPaths <-
      -- Get the list of children elements of @directory@
      (listDirectory $ toFilePath sourceDirectory)
        >>=
        -- Tries to parse the elements given by @listDirectory@ to relative directory paths
        -- and keep only successful entries
        pure . catMaybes . map parseRelDir
        >>=
        -- turn into absolute paths
        pure . map (sourceDirectory </>)
        >>=
        -- keep only directories that exists
        -- this also ensures that this list comprises directories only, see doc of @doesDirectoryExist@
        filterM (doesDirectoryExist . toFilePath)

    -- List of files inside
    filePaths <-
      -- Get the list of children elements of @sourceDirectory@
      (listDirectory $ toFilePath sourceDirectory)
        >>=
        -- Tries to parse the elements given by @listDirectory@ to relative directory paths
        -- and keep only successful entries
        pure . catMaybes . map parseRelFile
        >>=
        -- turn into absolute paths
        pure . map (sourceDirectory </>)
        >>=
        -- keep only directories that exists
        -- this also ensures that this list comprises files only, see doc of @doesFileExist@
        filterM (doesFileExist . toFilePath)

    -- Move directories and files
    mapM_ (uncurry moveOrCopy) [(toFilePath dirPath, toFilePath $ targetDirectory </> dirname dirPath) | dirPath <- dirPaths]
    mapM_ (uncurry moveOrCopy) [(toFilePath filePath, toFilePath $ targetDirectory </> filename filePath) | filePath <- filePaths]

    -- Finish
    return ()
  where
    -- Need to handle cases where the source directory is on a different disk than the destination directory since
    -- rename will fail in these cases.
    moveOrCopy srcFilePath destFilePath = renamePath srcFilePath destFilePath `catch` exdev srcFilePath destFilePath
    -- Stolen from: https://github.com/mihaimaruseac/hindent/issues/170
    exdev srcFilePath destFilePath e =
      if ioe_errno e == Just ((\(Errno a) -> a) eXDEV)
        then copyFile srcFilePath destFilePath >> removeFile srcFilePath
        else throw e