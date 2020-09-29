{-# LANGUAGE TypeApplications #-}

module System.Directory.Funflow (moveDirectoryContent) where

import Control.Monad (filterM)
import Data.Maybe (catMaybes)
import Path (Abs, Dir, Path, dirname, filename, parseRelDir, parseRelFile, toFilePath, (</>))
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, renamePath)

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
    mapM_ (uncurry renamePath) [(toFilePath dirPath, toFilePath $ targetDirectory </> dirname dirPath) | dirPath <- dirPaths]
    mapM_ (uncurry renamePath) [(toFilePath filePath, toFilePath $ targetDirectory </> filename filePath) | filePath <- filePaths]

    -- Finish
    return ()