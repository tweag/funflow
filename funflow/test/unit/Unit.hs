{-# LANGUAGE Arrows #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- Funflow unit tests

import Path (Abs, Dir, Rel, absdir, parseAbsDir, reldir, (</>))
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
  --testDir <- ioFlow (\() -> getCurrentDirectory >>= parseAbsDir) -< ()
  item <- putDirFlow -< testDir    -- get the CS.Item by piping through the temp folder
  pureFlow $ id -< item

getTempDirItem :: IO CS.Item
getTempDirItem = runFlow flow4Item ()

prop_DockerTaskInputMonoidLeftIdentity :: Property
prop_DockerTaskInputMonoidLeftIdentity = QCM.monadicIO $ do
    argsVals <- QCM.pick arbitrary
    paths <- QCM.pick (listOf arbitrary)
    item <- QCM.run getTempDirItem
    let vols       = [DT.VolumeBinding{ DT.item = item, DT.mount = p } | p <- paths]
        dockTaskIn = DT.DockerTaskInput{ DT.inputBindings = vols , DT.argsVals = argsVals }
    return (mempty <> dockTaskIn == dockTaskIn)

{-prop_DockerTaskInputMonoidRightIdentity :: DockerTaskInput -> Bool
prop_DockerTaskInputMonoidRightIdentity x = x <> mempty == x
-}

{-prop_DockerTaskInputMonoidAssociativity :: DockerTaskInput -> DockerTaskInput -> DockerTaskInput -> Bool
prop_DockerTaskInputMonoidAssociativity x y z = (x <> y) <> z == x <> (y <> z)
-}

main :: IO ()
main = do
    --quickCheck prop_DockerTaskInputMonoidAssociativity
    quickCheck prop_DockerTaskInputMonoidLeftIdentity
    --quickCheck prop_DockerTaskInputMonoidRightIdentity
    putStrLn "done!"
