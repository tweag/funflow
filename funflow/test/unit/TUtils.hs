module TUtils where

import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Path (Abs, Dir, Path, parseAbsDir)
import Test.QuickCheck

-- typical folder name characters
commonFolderChars :: [Char]
commonFolderChars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['-', '_'] ++ ['0' .. '9']

-- random String, typically for relative path
randomRel1 :: Gen String
randomRel1 = listOf1 (elements commonFolderChars)

-- arbitrary list of unique elements
uniqueGen :: Ord a => Gen [a] -> Gen [a]
uniqueGen = fmap (Set.toList . Set.fromList)

-- Make a String into an absolute path
unsafeToAbsDir :: String -> Path Abs Dir
unsafeToAbsDir = fromJust . parseAbsDir . ('/' :)
