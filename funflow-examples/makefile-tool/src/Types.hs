{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}

module Types where

import qualified Data.Set as Set

type Set = Set.Set

-- | Data Definitions
--------------------------------------------------------------------------------

-- | Name of source file
type SourceFile = String

-- | Name of target file
type TargetFile = String

-- | Either source xor target
type BuildFile = String

type Command = String

data MakeFile where
  MakeFile :: { sourceFiles :: Set SourceFile
              , defaultGoal :: MakeRule
              , allrules :: Set MakeRule
              } -> MakeFile
  deriving Show

data MakeRule where
  MakeRule :: TargetFile -> Set BuildFile -> Command -> MakeRule
  deriving (Eq, Ord, Show)

-- Makefile error type
newtype MFError = MFError String

-- | Accessors
--------------------------------------------------------------------------------
mkRuleTarNm :: MakeRule -> String
mkRuleTarNm (MakeRule tf _ _) = tf
