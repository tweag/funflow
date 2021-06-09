{-# LANGUAGE ScopedTypeVariables #-}

module Parse ( getValidMakeFile
             , parsecMakeFile
             , parseMakeFile
             , regularParse
             , testMakeFileParse
             )
where

import           Control.Applicative    (many, (<|>))
import Control.Exception (SomeException)
import Control.Exception.Safe (try)
import           Control.Monad          (guard, void)
import           Data.List              (nub)
import qualified Data.Set               as Set
import System.Directory (getCurrentDirectory)
import           Text.Parsec            (ParseError, parse)
import           Text.Parsec.Char       (char, letter, newline, noneOf, oneOf,
                                         string)
import           Text.Parsec.Combinator (many1)
import           Text.Parsec.String     (Parser)

-- Internal Imports
import           Types

-- | Top level code
--------------------------------------------------------------------
parseMakeFile :: String -> Either ParseError MakeFile
parseMakeFile = regularParse parsecMakeFile


-- | Parsec Stuff
--------------------------------------------------------------------
parsecMakeFile :: Parser MakeFile
parsecMakeFile = do
  srcFiles <- parsecSrcFiles
  (drule:rules) :: [MakeRule] <- many1 $ wspaceWrap parsecRule
  return $ MakeFile
    { sourceFiles = srcFiles
    , defaultGoal = drule
    , allrules = Set.fromList (drule:rules)
    }

parsecSrcFiles :: Parser (Set.Set SourceFile)
parsecSrcFiles = do
  whitespace
  void $ string "source-files:"
  rest <- many1 (noneOf ['\n'])
  let srcFiles = words rest
  void $ newline -- at the end
  guard $ length srcFiles == length (nub srcFiles)
  let sources = Set.fromList srcFiles
  return sources

parsecRule :: Parser MakeRule
parsecRule = do
  targetFile <- many1 (letter <|> char '.')
  void $ char ':'
  depStr <- many1 (noneOf ['\n'])
  let deps = words depStr
  guard $ length deps == length (nub deps)
  let buildSet = Set.fromList deps
  void newline
  command <- many1 (noneOf ['\n'])
  return $ MakeRule targetFile buildSet command

-- Taken from the parsec tutorial:
regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

wspaceWrap :: Parser a -> Parser a
wspaceWrap p = do
  whitespace
  a <- p
  whitespace
  return a

-- Taken from the parsec tutorial:
whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"
--------------------------------------------------------------------


-- | Extract valid makefile at given path, else @$PWD/Makefile@
--   Result is either a @Left@-wrapped error message or a @Right@-wrapped result.
getValidMakeFile :: Maybe FilePath -> IO (Either MakeFile MFError)
getValidMakeFile optDir = do
  readEither <- tryReadMakeFile optDir
  case readEither of
    Left errMsg ->
      return $ Right $ MFError errMsg
    Right fileRead ->
      case parseMakeFile fileRead of
        Left errorMsg ->
          return $ Right $ MFError $ show errorMsg
        Right mkFile ->
          -- for now, ignoring checking it's valid
          return $ Left mkFile

-- Attempt to parse Makefile at given path, else @$PWD/Makefile@
-- Result is either a @Left@-wrapped error message or a @Right@-wrapped result.
tryReadMakeFile :: Maybe FilePath -> IO (Either String String)
tryReadMakeFile optDir = do
  dir <- dirOrCwd optDir
  let makeFileLoc = dir ++ "/Makefile"
  tryRead <- try $ readFile makeFileLoc
  case tryRead of
    Left (_ :: SomeException) -> return (Left makeFileLoc)
    Right file -> return (Right file)

-- IO context of given path, else $PWD
dirOrCwd :: Maybe FilePath -> IO FilePath
dirOrCwd Nothing = getCurrentDirectory
dirOrCwd (Just d) = return d

-- | Strictly testing
--------------------------------------------------------------------------------
testMakeFileParse :: Maybe FilePath -> IO ()
testMakeFileParse optDir = do
  Right mkfilestr <- tryReadMakeFile optDir
  putStrLn "Testing make file parsing:"
  putStrLn "Readfile:"
  putStrLn mkfilestr
  putStrLn "Parsing:"
  print $ regularParse parsecMakeFile mkfilestr
