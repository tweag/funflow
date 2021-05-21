{-# LANGUAGE FlexibleInstances #-}

module TInstances where

import Data.Maybe (fromJust)
import qualified Data.Text as Text
import Path (Abs, Dir, Path, parseAbsDir)
import Test.QuickCheck

instance Arbitrary (Path Abs Dir) where
    arbitrary = let pickChar = elements (['a'..'z'] ++ ['A'..'Z'] ++ ['-', '_'] ++ ['0'..'9'])
                    chars    = choose (1,5) >>= (\k -> vectorOf k pickChar)
                    genDir   =  ('/':) <$> chars
                in fromJust . parseAbsDir <$> genDir

instance Arbitrary Text.Text where
    arbitrary = Text.pack <$> arbitrary
