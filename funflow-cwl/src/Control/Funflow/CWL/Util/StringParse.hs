{-# LANGUAGE GADTs      #-}
{-# LANGUAGE Strict     #-}
{-# LANGUAGE StrictData #-}


{-|
Module      : StringParse
Description : Parsec parsing functions

This module exports various string parsing functions
so that a bit of parsec boilerplate is all in one place.
-}


module Control.Funflow.CWL.Util.StringParse
  ( parseSourceRef
  , parseGlobStr
  , parseDestRef
  ) where

-- Parsing
import           Control.Monad
    (void)
import           Text.Parsec
    (parse, (<|>))
import           Text.Parsec.Char
    (alphaNum, char, string)
import           Text.Parsec.Combinator
    (eof, many1)
import           Text.Parsec.Prim
    (try)
import           Text.Parsec.String
    (Parser)

-- Internal imports
import Data.ErrorMonad



-- * Exposed Interface
--------------------------------------------------------------------------------

-- | Parses strings of the form "$(inputs.x)" to "x"
parseGlobStr :: String -> Maybe String
parseGlobStr = maybeParse parsecGlobString


-- | Parses strings appearing in as a source
-- in a workflow.
parseSourceRef :: String -> ErrM String
parseSourceRef = runParser parsecSourceRef

-- | Parses source strings like "test/one" into
-- "one" or, if no such source exists, it parses
-- "one" into "one"
parseDestRef :: String -> Maybe String
parseDestRef = maybeParse parsecDestRef



-- * Parsing
--------------------------------------------------------------------------------

-- | Parse a JS input parameter reference.
parsecGlobString :: Parser String
parsecGlobString = do
  void $ string "$(inputs."
  str <- many1 globChar
  void $ char ')'
  eof
  return str


-- | Parse Workflow Source references
-- of the form "someOtherstep/outputId" or
-- "workflowInputId".
parsecSourceRef :: Parser String
parsecSourceRef = try parseWfInSourceRef <|> parseWfStepSourceRef

  where
    -- | Workflow sources that refer to other steps.
    parseWfStepSourceRef :: Parser String
    parseWfStepSourceRef = do
      stepId <- many1 refNameChar
      void $ char '/'
      void $ many1 refNameChar
      eof
      return stepId


-- | The unqualified name of the output, which might not be unique per
-- workflow.
parsecDestRef :: Parser String
parsecDestRef = try parseWfStepSourceDest <|> parseWfInSourceRef
  where

  -- | The output of Workflow sources
  -- that refer to other steps.
  parseWfStepSourceDest :: Parser String
  parseWfStepSourceDest = do
    void $ many1 refNameChar
    void $ char '/'
    outputName <- many1 refNameChar
    eof
    return outputName


-- | Workflow sources that refer to inputs
parseWfInSourceRef :: Parser String
parseWfInSourceRef = do
  idRef <- many1 refNameChar
  eof
  return idRef





-- *  Lib
--------------------------------------------------------------------------------

runParser :: Parser a -> String -> ErrM a
runParser p = injectEitherErr . parse p ""

maybeParse :: Parser a -> String -> Maybe a
maybeParse p x = case parse p "" x of
  Left _  -> Nothing
  Right a -> Just a

-- | This is any character that can appear in a
-- unique identifier (and hence, can appear in 
-- a source reference).
refNameChar :: Parser Char
refNameChar = alphaNum <|> (char '_') <|> (char '-')

-- | These are valid glob characters.
globChar :: Parser Char
globChar = foldl (<|>) refNameChar
  [ (char '*')
  , (char '^')
  , (char '?')
  , (char '!')
  , (char '[')
  , (char ']')
  ]














