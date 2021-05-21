{-# LANGUAGE Arrows #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- Funflow unit tests

import Path (parseAbsDir, reldir, (</>))
import System.Directory (getCurrentDirectory)
import Test.QuickCheck
import qualified Test.QuickCheck.Monadic as QCM

import qualified Data.CAS.ContentStore as CS
import Funflow (Flow, ioFlow, pureFlow, putDirFlow, runFlow)
import qualified Funflow.Tasks.Docker as DT
import TInstances()


flow4Item :: Flow () CS.Item
flow4Item = proc () -> do
  -- Note: the relative path is specific to running the test with Nix with `$(nix-build nix -A funflow.components.tests)/bin/test-funflow`
  --       which is the case in the CI
  testDir <- ioFlow (\() -> return . flip (</>) [reldir|./funflow/test/flows/assets/storeFlowTest/|] =<< parseAbsDir =<< getCurrentDirectory) -< ()
  item <- putDirFlow -< testDir    -- get the CS.Item by piping through the temp folder
  pureFlow $ id -< item

getTempDirItem :: IO CS.Item
getTempDirItem = runFlow flow4Item ()

buildDockerTaskInput :: QCM.PropertyM IO DT.DockerTaskInput
buildDockerTaskInput = do
    argsVals <- QCM.pick arbitrary
    mountPaths <- QCM.pick (listOf arbitrary)
    item <- QCM.run getTempDirItem
    return DT.DockerTaskInput{ 
        DT.inputBindings = [DT.VolumeBinding{ DT.item = item, DT.mount = p } | p <- mountPaths] , 
        DT.argsVals = argsVals
    }

checkOneInput :: (DT.DockerTaskInput -> Bool) -> Property
checkOneInput testPred = QCM.monadicIO $ testPred <$> buildDockerTaskInput

prop_DockerTaskInputMonoidLeftIdentity :: Property
prop_DockerTaskInputMonoidLeftIdentity = checkOneInput (\dti -> mempty <> dti == dti)

prop_DockerTaskInputMonoidRightIdentity :: Property
prop_DockerTaskInputMonoidRightIdentity = checkOneInput (\dti -> dti == dti <> mempty)

prop_DockerTaskInputMonoidAssociativity :: Property
prop_DockerTaskInputMonoidAssociativity = QCM.monadicIO $ do
    x <- buildDockerTaskInput
    y <- buildDockerTaskInput
    z <- buildDockerTaskInput
    return ((x <> y) <> z == x <> (y <> z))

main :: IO ()
main = do
    quickCheck prop_DockerTaskInputMonoidLeftIdentity
    quickCheck prop_DockerTaskInputMonoidRightIdentity
    quickCheck prop_DockerTaskInputMonoidAssociativity
    putStrLn "done!"
