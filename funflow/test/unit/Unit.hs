{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleInstances #-}

-- Funflow unit tests

import "cryptonite" Crypto.Hash (digestFromByteString, SHA256, Digest)
import qualified Data.ByteString as BS
import Data.Maybe (fromJust, isJust)
import qualified Data.Text as Text
import Data.Word (Word8)
import Path (Abs, Dir, Path (..), fromAbsDir, parseAbsDir)
import Test.QuickCheck

import qualified Data.CAS.ContentStore as CS
import qualified Data.CAS.ContentHashable as CH
import qualified Funflow.Tasks.Docker as DT
import Funflow.Tasks.Docker (DockerTaskInput)

genBsForSHA256 :: Gen BS.ByteString
genBsForSHA256 = BS.pack <$> vectorOf 8 (arbitrary :: Gen Word8)

instance Arbitrary CS.Item where
    arbitrary = CS.Item . fromJust <$> fmap CH.fromBytes genBsForSHA256 `suchThat` isJust

genDir :: Gen FilePath
genDir = 
    let pickChar = elements (['a'..'z'] ++ ['A'..'Z'] ++ ['-', '_'] ++ ['0'..'9'])
        chars = listOf pickChar `suchThat` (not . null)
    in ('/':) <$> chars

instance Arbitrary (Path Abs Dir) where
    arbitrary = fromJust . parseAbsDir <$> genDir

instance Arbitrary DT.VolumeBinding where
    arbitrary = (\(it, mnt) -> DT.VolumeBinding{ DT.item = it, DT.mount = mnt }) <$> do
        it <- arbitrary
        mnt <- arbitrary
        return (it, mnt)

instance Arbitrary Text.Text where
    arbitrary = Text.pack <$> arbitrary

boundedListOf :: (Int -> Int -> Int) -> Int -> Gen a -> Gen [a]
boundedListOf f m g = sized $ \n -> 
	do k <- choose (0, f m n)
	   vectorOf k g

maxSizeListOf :: Int -> Gen a -> Gen [a]
maxSizeListOf = boundedListOf min

minSizeListOf :: Int -> Gen a -> Gen [a]
minSizeListOf = boundedListOf max

instance Arbitrary DockerTaskInput where
    arbitrary = ( \(bs, as) -> DT.DockerTaskInput{ DT.inputBindings = bs , DT.argsVals = as } ) <$> do
        bindings <- maxSizeListOf 0 arbitrary
        args     <- arbitrary
        return (bindings, args)

prop_DockerTaskInputMonoidAssociativity :: DockerTaskInput -> DockerTaskInput -> DockerTaskInput -> Bool
prop_DockerTaskInputMonoidAssociativity x y z = (x <> y) <> z == x <> (y <> z)

main :: IO ()
main = do
	quickCheckWith stdArgs { maxSuccess = 100, maxDiscardRatio = 10 } prop_DockerTaskInputMonoidAssociativity
	--quickCheck prop_DockerTaskInputMonoidLeftIdentity
	--quickCheck prop_DockerTaskInputMonoidRightIdentity
	putStrLn "done!"
