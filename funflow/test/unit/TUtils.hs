module TUtils where

import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Path (Abs, Dir, Path, parseAbsDir)
import Test.QuickCheck

commonFolderChars :: [Char]
commonFolderChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['-', '_'] ++ ['0'..'9']

randomRel1 :: Gen String
randomRel1 = listOf1 (elements commonFolderChars)

uniqueGen :: Ord a => Gen [a] -> Gen [a]
uniqueGen = fmap (Set.toList . Set.fromList)

unsafeToAbsDir :: String -> Path Abs Dir
unsafeToAbsDir = fromJust . parseAbsDir . ('/':)
