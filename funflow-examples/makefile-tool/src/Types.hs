{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}

module Types where

import qualified Data.Set as Set

type Set = Set.Set

-- | Data Definitions
--------------------------------------------------------------------------------

type SourceFile = String     -- | Name of source file
type TargetFile = String     -- | Name of target file
type BuildFile  = String     -- | Either source xor target

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

-- Makefile error type
newtype MFError = MFError String



-- | Accessors
--------------------------------------------------------------------------------

mkRuleTarNm :: MakeRule -> String
mkRuleTarNm (MakeRule tf _ _) = tf


