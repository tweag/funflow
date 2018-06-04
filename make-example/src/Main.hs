{-# LANGUAGE Arrows            #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE  QuasiQuotes      #-}
{-# LANGUAGE  ScopedTypeVariables #-}

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
import Text.Parsec.Char (noneOf, oneOf, char, digit, satisfy, newline, string,
                        letter)
import Text.Parsec.Combinator (many1, choice, chainl1)

import Control.Monad (void, guard)
import Control.Applicative ((<$>), (<*>), (<*), (*>), many, (<|>))
import Data.Char (isLetter, isDigit)

import System.Posix.Files ( setFileMode, accessModes )

type Set = Set.Set


{-

# TODO Later
1) Clean import list.
2) Check validity of make file.


-}



-------------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------------

main :: IO ()
main = do
    perhapsMakeFile <- getValidMakeFile
    case perhapsMakeFile of
      Right (MFError err) -> putStrLn $ "Invalid make file: \n" ++ err
      Left mfile -> do
        let defGoalRule = defaultGoal mfile
        cwd <- getCurrentDir
        r <- withSimpleLocalRunner (cwd </> [reldir|makefiletest/store|]) $ \run ->
          run ((buildTarget mfile defGoalRule) >>> readString) ()
          -- readString :: SimpleFlow (Content File) String
        case r of
          Left error ->
            putStrLn $ "\n\nFailed, target failed: \n\n" ++ displayException error
          Right (execContent :: String) -> do
            let outpath = ((fromAbsDir cwd) ++ "/a.out")
            writeFile outpath execContent
            setFileMode outpath accessModes
            putStrLn $ "\n\nSuccess, target executable made."



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
buildTarget mkfile target@(MakeRule targetNm deps cmd) = proc _ -> do
  let srcfiles = sourceFiles mkfile
  let allRules = allrules mkfile
  () <- guardFlow -< (target `Set.member` allRules) -- | Does this work?
  let neededTargets = Set.toList $ deps `Set.difference` srcfiles
  let neededSources  = Set.toList $ deps `Set.intersection` srcfiles
  let neededTargetRules = findRules mkfile neededTargets
  case neededTargetRules of
    Nothing -> do
      () <- failNow -< ()
      returnA -< (error "dependency rules missing")
    Just (depRules :: [MakeRule]) -> do
      -- really cool application: no repeated work here!
      -- a powerful dynamic programming tool
      let depTargetFlows = map (buildTarget mkfile) depRules
      let countDepFlows = length depTargetFlows
      joinedFlow <- flowJoin -< depTargetFlows
      depFiles <- joinedFlow -< (replicate countDepFlows ())
      compiledFile <- compileFile -< (targetNm, neededSources, depFiles)
      returnA -< compiledFile


compileFile = undefined

findRules = undefined

-- Do we want this? How does it work with error handling?
data ExitStatus where
  NoError :: ExitStatus
  Error   :: ExitStatus



---------------------------------------------------------------------------------



-- | Reference
---------------------------------------------------------------------------------
getMkRuleTargetNm :: MakeRule -> TargetFile
getMkRuleTargetNm (MakeRule tf _ _) = tf

-- uggh, need lenght indexed vectors to do this nicely
flowJoin :: SimpleFlow [SimpleFlow a b] (SimpleFlow [a] [b])
flowJoin = proc flows -> do
  case flows of
    [] -> returnA -< (step $ \_ -> [])
    (f:fs) -> do
      joinedFlow <- flowJoin -< fs
      returnA -< (proc (a:as) -> do
                     b <- f -< a
                     bs <- joinedFlow -< as
                     returnA -< (b:bs)
                    )

{-
flowJoin [] = step (\_ -> [])                           -- Eh.
flowJoin ff@(f:fs) = proc aa@(a:as) -> do
  () <- guardFlow -< (length ff == length aa)
  b <- f -< a
  bs <- (flowJoin fs) -< as
  returnA -< (b:bs)
-}

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




