{-# LANGUAGE ScopedTypeVariables #-}

module Parse ( regularParse
             , parsecMakeFile
             , parseMakeFile
             )
where

import           Control.Applicative    (many, (<|>))
import           Control.Monad          (guard, void)
import           Data.List              (nub)
import           Text.Parsec            (ParseError, parse)
import           Text.Parsec.Char       (char, letter, newline, noneOf, oneOf,
                                         string)
import           Text.Parsec.Combinator (many1)
import           Text.Parsec.String     (Parser)

-- Internal Imports
import           Types

import qualified Data.Set               as Set


-- | Top level code
--------------------------------------------------------------------

parseMakeFile :: String -> Either ParseError MakeFile
parseMakeFile = regularParse parsecMakeFile

-- Taken from the parsec tutorial:
regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

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


wspaceWrap :: Parser a -> Parser a
wspaceWrap p = do
  whitespace
  a <- p
  whitespace
  return a

-- Taken from the parsec tutorial:
whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"


