{-# LANGUAGE Arrows              #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main where

-- Funflow
import           Control.Arrow
import           Control.Arrow.Free
import           Control.Exception.Safe ( try )
import           Control.Funflow
import qualified Control.Funflow.ContentStore                as CS
import           Control.Funflow.External.Coordinator.Memory
import           Control.Funflow.Pretty
import Control.Funflow.ContentStore (Content (..))
import Control.Exception (Exception (..), SomeException (..))
import qualified Control.Funflow.External.Docker as Docker
import Data.List.Unique

import Path.IO
import Path
import Data.Default
import Data.Monoid  ((<>))

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Set as Set

import Debug.Trace (trace)

import System.IO
import System.Directory
import Text.Read (readMaybe)

import Text.Parsec (ParseError, parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (noneOf, oneOf, char, digit,
                         satisfy, newline, string, letter)
import Text.Parsec.Combinator (many1, choice, chainl1)

import Control.Monad (void, guard)
import Control.Applicative ((<$>), (<*>), (<*), (*>), many, (<|>))
import Data.Char (isLetter, isDigit)

import System.Posix.Files ( setFileMode, accessModes )
import qualified Data.ByteString as BS

type Set = Set.Set
type Map = Map.Map


{-

# TODO Later
1) Clean import list.
2) Check validity of make file.
3) Use
   > type (==>) = SimpleFlow
   everywhere.

-}



-- | Main
-------------------------------------------------------------------------------------

main :: IO ()
main = do
    perhapsMakeFile <- getValidMakeFile
    case perhapsMakeFile of
      Right (MFError err) -> putStrLn $ "Invalid make file: \n" ++ err
      Left mfile -> do
        let defGoalRule = defaultGoal mfile
        let tfName = getRuleTargetNm defGoalRule
        cwd <- getCurrentDir
        r <- withSimpleLocalRunner (cwd </> [reldir|makefiletest/store|]) $ \run ->
          run ((buildTarget mfile defGoalRule) >>> readByteString) ()
        case r of
          Left error ->
            putStrLn $ "\n\nFailed, target failed: \n\n" ++ displayException error
          Right (execContent :: BS.ByteString) -> do
            let outpath = (fromAbsDir cwd) ++ "/" ++ tfName
            BS.writeFile outpath execContent
            setFileMode outpath accessModes -- chmod +x
            putStrLn "\n\nSuccess, target executable made."


readByteString :: Content File ==> BS.ByteString
readByteString = getFromStore $ BS.readFile . fromAbsFile

-- | Data Defs
-------------------------------------------------------------------------------------

type SourceFile = String           -- | Name of source file
type TargetFile = String           -- | Name of target file
type BuildFile = String            -- | Either source xor target

data MakeFile where
  MakeFile :: { sourceFiles :: Set SourceFile
              , defaultGoal :: MakeRule
              , allrules :: Set MakeRule
              } -> MakeFile
  deriving Show

data MakeRule where
  MakeRule :: TargetFile -> Set BuildFile -> Command -> MakeRule
  deriving (Eq, Ord, Show)

type Command = String

newtype MFError = MFError String

-------------------------------------------------------------------------------------



-- | Extract valid make file
-----------------------------------------------------------------------------------------

getValidMakeFile :: IO (Either MakeFile MFError)
getValidMakeFile = do
  tryRead <- readMakeFileMaybe
  case tryRead of
    Nothing ->
      return $ Right $ MFError $ "`cwd`/makefile does not exist"
    Just readFile ->
      case parseMakeFile readFile of
        Left error ->
          return $ Right $ MFError $ show error
        Right mkFile ->
          -- for now, ignoring checking it's valid
          return $ Left mkFile


readMakeFileMaybe :: IO (Maybe String)
readMakeFileMaybe = do
  cwd <- getCurrentDirectory
  let makeFileLoc = cwd ++ "/makefile"
  tryRead <- try $ readFile makeFileLoc
  case tryRead of
    Left (_ :: SomeException) -> return Nothing
    Right file -> return (Just file)


-----------------------------------------------------------------------------------------


-- | Parsing
--------------------------------------------------------------------------------

parseMakeFile :: String -> Either ParseError MakeFile
parseMakeFile = regularParse parsecMakeFile

parsecMakeFile :: Parser MakeFile
parsecMakeFile = do
  srcFiles <- parsecSrcFiles
  (drule:rules) :: [MakeRule] <- many1 $ wspaceWrap parsecRule
  return $ MakeFile
    { sourceFiles = srcFiles
    , defaultGoal = drule
    , allrules = Set.fromList (drule:rules)
    }

parsecSrcFiles :: Parser (Set SourceFile)
parsecSrcFiles = do
  whitespace
  void $ string "source-files:"
  rest <- many1 (noneOf ['\n'])
  let srcFiles = words rest
  void $ newline -- at the end
  guard $ allUnique srcFiles
  let sources = Set.fromList srcFiles
  return sources

parsecRule :: Parser MakeRule
parsecRule = do
  targetFile <- many1 (letter <|> char '.')
  void $ char ':'
  depStr <- many1 (noneOf ['\n'])
  let deps = words depStr
  guard $ allUnique deps
  let buildSet = Set.fromList deps
  void newline
  command <- many1 (noneOf ['\n'])
  return $ MakeRule targetFile buildSet command


wspaceWrap :: Parser a -> Parser a
wspaceWrap p = do
  whitespace
  a <- p
  whitespace
  return a

-- tutorial online:
whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

-- tutorial online:
regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

maybeParseMakeFile :: String -> Maybe MakeFile
maybeParseMakeFile src =
  case regularParse parsecMakeFile src of
    Left _ -> Nothing
    Right mkFile -> return mkFile

testMakeFileParse :: IO ()
testMakeFileParse = do
  Just mkfilestr <- readMakeFileMaybe
  putStrLn "Testing make file parsing:"
  putStrLn "Readfile:"
  putStrLn mkfilestr
  putStrLn "Parsing:"
  putStrLn $ show $ regularParse parsecMakeFile mkfilestr

--------------------------------------------------------------------------------




-- | Build a target
---------------------------------------------------------------------------------
-- assuming the makefile is valid at this point!

buildTarget :: MakeFile -> MakeRule -> SimpleFlow () (Content File)
buildTarget mkfile target@(MakeRule targetNm deps cmd) = let
   srcfiles = sourceFiles mkfile
   neededTargets = Set.toList $ deps `Set.difference` srcfiles
   neededSources  = Set.toList $ deps `Set.intersection` srcfiles
   neededTargetRules = findRules mkfile neededTargets
 in case neededTargetRules of
   Nothing -> proc _ -> do
     () <- failNow -< ()
     returnA -< (error "dependency rules missing")
   Just (depRules :: [MakeRule]) -> let
       depTargetFlows = map (buildTarget mkfile) depRules
       countDepFlows = length depTargetFlows
       grabSources srcs = monadJoin $ map (readFile . ("./" ++)) srcs
       grabSrcsFlow = stepIO grabSources
     in proc _ -> do
       -- really cool application: no repeated work here!
       -- a powerful dynamic programming tool
       () <- guardFlow -< (target `Set.member` (allrules mkfile)) -- | Does this work?
       contentSrcFiles <- grabSrcsFlow -< neededSources
       let fullSrcFiles = Map.fromList $ zip neededSources contentSrcFiles
       depFiles <- flowJoin depTargetFlows -< (replicate countDepFlows ())
       compiledFile <- compileFile -< (targetNm, fullSrcFiles, depFiles,cmd)
       returnA -< compiledFile

-- Does grabSrcsFlow fail if one can't be found?

monadJoin :: Monad m => [m a] -> m [a]
monadJoin [] = return []
monadJoin (m:ms) = do
  a <- m
  as <- monadJoin ms
  return (a:as)

findRules :: MakeFile -> [TargetFile] -> Maybe [MakeRule]
findRules MakeFile {allrules = rules} ts = do
  guard ((tfileSet Set.\\ ruleTarNmSet) == Set.empty)
  let targetRules = Set.filter ((`Set.member` tfileSet) . getRuleTargetNm) rules
  return $ Set.toList targetRules
  where
    ruleTarNmSet = Set.map getRuleTargetNm rules
    tfileSet = Set.fromList ts

getRuleTargetNm :: MakeRule -> String
getRuleTargetNm (MakeRule tf _ _) = tf


type (==>) = SimpleFlow

-- | Compiles a C file in a docker container.
compileFile ::
  (TargetFile, Map SourceFile String, [Content File], Command) ==> (Content File)
compileFile = proc (tf, srcDeps, tarDeps, cmd) -> do
  srcsInStore <- writeToStore -< srcDeps
  let inputFilesInStore = srcsInStore ++ tarDeps
  inputDir <- mergeFiles -< inputFilesInStore
  let scriptSrc = "#!/usr/bin/env bash\n\
                  \cd /input/deps\n" ++ cmd ++
                  "\ncp " ++ tf ++ " /output/"
  compileScript <- writeExecutableString -< (scriptSrc, [relfile|script.sh|])
  compiledFile <- dockerFlow -< (inputDir,compileScript)
  relpathCompiledFile <- flowStringToRelFile -< tf
  returnA -< (compiledFile :</> relpathCompiledFile)
    where
      dockerFlow = docker dockerConfFn
      dockerConfFn (depDir, compileScript) = Docker.Config
        { Docker.image = "gcc"
        , Docker.optImageID = Nothing
        , Docker.input = Docker.MultiInput inputs
        , Docker.command = "./input/script/script.sh"
        , Docker.args = []
        }
        where
          inputs = Map.fromList [scriptInput, depInput]
          mkInputPath = IPItem . CS.contentItem
          scriptInput = ("script", mkInputPath compileScript)
          depInput = ("deps", mkInputPath depDir)



type FileName = String
type FileContent = String

writeToStore :: (Map FileName FileContent) ==> [Content File]
writeToStore = proc files -> do
  let listfiles = Map.toList files
  filesWithPaths <- mapA flowfixSrcFileData -< listfiles
  contents <- mapA writeString -< filesWithPaths
  returnA -< contents


-- These three functions can be cleaned up.
ioFixSrcFileData :: (FileName, FileContent) -> IO (FileContent, Path Rel File)
ioFixSrcFileData (x,y) = do
  path <- parseRelFile x -- parseRelFile :: FileName -> IO Path Rel File
  return (y,path)

flowfixSrcFileData :: (String, String) ==> (String, Path Rel File)
flowfixSrcFileData = stepIO fix

flowStringToRelFile :: String ==> Path Rel File
flowStringToRelFile = stepIO parseRelFile

{-
# Process

1) Get source files and make a Map SourceFile String for name -> content.
2) Write each of these in the store with their name as the relpath.
3) Create a docker config:

a) gcc
b) Nothing
c) Input: use the contentItem below to get all the Content Files you have and store those.

-- contentItem :: Content t -> Item

Also, a bash script (made executable with writeExecutableString) called
"bashscript.sh"

This should simply
   * go to /input
   * run the input command from the makefile
   * copy the expected file to /output/

You'll need to put each of these in their own folder (or some shared folders).
All the dep files can go in /input/deps/.
The script can go in /input/script.

d) The command to run the bash script: "./input/script/bashscript.sh"


# Docker Containers

As I understand it, all we do is run a bash script
in a docker container. We've preloaded files from the cache into
/input/ and when we're done, we look for a (already specified) file in
/output/, put that in the cache and return a CS.Item for that file in the cache.

-}


-- Do we want this? How does it work with error handling?
data ExitStatus where
  NoError :: ExitStatus
  Error   :: ExitStatus



---------------------------------------------------------------------------------



-- | Reference/ Library
---------------------------------------------------------------------------------

flowJoin :: [SimpleFlow a b] -> SimpleFlow [a] [b]
flowJoin [] = step (\_ -> [])                           -- Eh.
flowJoin ff@(f:fs) = proc aa@(a:as) -> do
  () <- guardFlow -< (length ff == length aa)
  b <- f -< a
  bs <- (flowJoin fs) -< as
  returnA -< (b:bs)


-- | Not sure if this works:
guardFlow :: SimpleFlow Bool ()
guardFlow = proc test -> do
  case test of
    True -> returnA -< ()
    False -> do
      () <- failNow -< ()
      returnA -< ()

failNow :: SimpleFlow () ()
failNow = proc _ -> do
      (stepIO err) -< ()

err :: () -> IO ()
err () = do
  error "error"

printFlow :: String ==> ()
printFlow = proc s -> do
  () <- stepIO putStrLn -< s
  returnA -< ()

---------------------------------------------------------------------------------


--  Reference for me
--------------------------------------------------------------------------------
runSFailFlow :: IO ()
runSFailFlow = do
  cwd <- getCurrentDir
  r <- withSimpleLocalRunner (cwd </> [reldir|example|]) $ \run -> do
    run simpleFailFlow 3
  case r of
    Left err ->
      putStrLn $ "\n\nFAILED: " ++ displayException err
    Right _ ->
      putStrLn $ "\n\nSUCCESS."

simpleFailFlow :: SimpleFlow Int ()
simpleFailFlow = proc _ -> do
  let x = True
  case x of
    True -> do
      (stepIO err) -< ()
      --returnA -< ()
    False -> do
      returnA -< ()
--------------------------------------------------------------------------------




