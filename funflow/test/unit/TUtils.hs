module TUtils where

import Test.QuickCheck
import Path (Abs, Dir, Path,Rel,  mkRelDir, (</>))

commonFolderChars :: [Char]
commonFolderChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['-', '_'] ++ ['0'..'9']

randomRel1 :: Gen String
randomRel1 = listOf1 (elements commonFolderChars)

{-randomAbs1 :: Path Abs Dir -> Gen (Path Abs Dir)
randomAbs1 p = (p </>) <$> randomRel1
-}