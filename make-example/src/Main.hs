{-# LANGUAGE Arrows            #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE  QuasiQuotes      #-}

module Main where

import           Control.Arrow
import           Control.Arrow.Free
import           Control.Exception.Safe
import           Control.Funflow
import qualified Control.Funflow.ContentStore                as CS
import           Control.Funflow.External.Coordinator.Memory
import           Control.Funflow.Pretty
import           Data.Monoid                                 ((<>))
import Path.IO
import Path
import Data.Default
import System.IO (stdout, hFlush)
import Text.Read (readMaybe)


import Control.Funflow.ContentStore (Content (..))
import Control.Exception (Exception (..))
import qualified Control.Funflow.External.Docker as Docker
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

{-

# TODO
1) Clean import list

-}

-------------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------------

main :: IO ()
main = do
    cwd <- getCurrentDir
    r <- withSimpleLocalRunner (cwd </> [reldir|makefiletest/store|]) $ \run -> do
      run mainFlow ()
    case r of
      Left err ->
        putStrLn $ "\n\nFailed, target failed: \n\n" ++ displayException err
      Right out ->
        putStrLn $ "\n\nSuccess, target made."

mainFlow :: SimpleFlow () ()
mainFlow = proc () -> do
  mfile <- parseMakeFile -< ()
  mfileChecked <- checkMakeFileValid -< mfile
  let defGoal = defaultGoal mfileChecked
  content <- buildTarget -< (mfileChecked, defGoal)
  -- then write it to file
  returnA -< ()

-------------------------------------------------------------------------------------
-- Data Defs
-------------------------------------------------------------------------------------

type File = undefined
type MFile = Either File String

data MakeFile where
  MakeFile : { sourceFiles :: Set File
             , defaultGoal :: MakeRule
             , allrules :: Set MakeRule
             } -> MakeFile

data MakeRule where
  MakeRule :: String -> Set MFile -> MakeRule
  deriving (Eq, Ord)


-------------------------------------------------------------------------------------
-- Core Work
-------------------------------------------------------------------------------------



parseMakeFile :: SimpleFlow () MakeFile
parseMakeFile = undefined
{-

-}


checkMakeFileValid :: SimpleFlow MakeFile MakeFile
checkMakeFileValid = undefined

{-

-}

buildTarget :: SimpleFlow (MakeFile, MakeRule) (Content File)
buildTarget = undefined















