{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleInstances #-}

module TInstances where

import "cryptonite" Crypto.Hash
import qualified Data.ByteString as BS
import Data.Maybe (fromJust)
import qualified Data.Text as Text
import Data.Word (Word8)
import Path (Abs, Dir, Path (..), parseAbsDir)
import Test.QuickCheck

import qualified Data.CAS.ContentStore as CS
import qualified Data.CAS.ContentHashable as CH
import qualified Funflow.Tasks.Docker as DT
import Funflow.Tasks.Docker (DockerTaskInput)

import TUtils (maxSizeListOf)

instance Arbitrary CS.Item where
    arbitrary = CS.Item . fromJust . CH.fromBytes <$> genBsForSHA256
        where genBsForSHA256 = BS.pack <$> vectorOf (hashDigestSize SHA256) arbitrary

instance Arbitrary (Path Abs Dir) where
    arbitrary = let pickChar = elements (['a'..'z'] ++ ['A'..'Z'] ++ ['-', '_'] ++ ['0'..'9'])
                    chars    = choose (1,5) >>= (\k -> vectorOf k pickChar)
                    genDir   =  ('/':) <$> chars
                in fromJust . parseAbsDir <$> genDir

instance Arbitrary DT.VolumeBinding where
    arbitrary = (\(it, mnt) -> DT.VolumeBinding{ DT.item = it, DT.mount = mnt }) <$> do
        it <- arbitrary
        mnt <- arbitrary
        return (it, mnt)

instance Arbitrary Text.Text where
    arbitrary = Text.pack <$> arbitrary

instance Arbitrary DockerTaskInput where
    arbitrary = ( \(bs, as) -> DT.DockerTaskInput{ DT.inputBindings = bs , DT.argsVals = as } ) <$> do
        bindings <- maxSizeListOf 5 arbitrary
        args     <- arbitrary
        return (bindings, args)
