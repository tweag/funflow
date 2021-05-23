module TUtils where

import Test.QuickCheck

commonFolderChars :: [Char]
commonFolderChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['-', '_'] ++ ['0'..'9']

randomRel1 :: Gen String
randomRel1 = listOf1 (elements commonFolderChars)
