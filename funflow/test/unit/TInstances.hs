{-# LANGUAGE FlexibleInstances #-}

module TInstances where

import Data.Maybe (fromJust)
import qualified Data.Text as Text
import Path (Abs, Dir, Path, parseAbsDir)
import Test.QuickCheck
import TUtils (commonFolderChars)

instance Arbitrary (Path Abs Dir) where
    arbitrary = let makeName k = vectorOf k (elements commonFolderChars)
                    chars      = choose (1,10) >>= makeName
                    genDir     =  ('/':) <$> chars
                in fromJust . parseAbsDir <$> genDir

instance Arbitrary Text.Text where
    arbitrary = Text.pack <$> arbitrary
