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

{-
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
-}

import qualified Data.CAS.ContentHashable as CH
import Path (Abs, Dir, Path, parseAbsDir)
import Test.Tasty
import Test.Tasty.QuickCheck (testProperties)
import qualified Data.CAS.RemoteCache as RC
import System.Directory (getCurrentDirectory)
import Path ((</>))

myNoOp :: Path Abs Dir -> CH.DirectoryContent -> IO ()
myNoOp _ _ = return ()

myNoErr :: CH.ContentHash -> IO ()
myNoErr _ = error "uh-oh!"

buildStore :: IO CS.ContentStore
buildStore = do
    --cwd <- getCurrentDirectory >>= parseAbsDir
    cwd <- parseAbsDir "/home/vr/sandbox"
    CS.open ( cwd </> [reldir|./tmp/store|] )

makeItem :: CS.ContentStore -> Path Abs Dir -> IO CS.Item
makeItem store p = CS.putInStore store RC.NoCache myNoErr myNoOp (CH.DirectoryContent p)

buildDockerTaskInput :: IO CS.ContentStore -> QCM.PropertyM IO DT.DockerTaskInput
buildDockerTaskInput getStore = do
    store      <- QCM.run getStore
    argsVals   <- QCM.pick arbitrary
    mountPaths <- QCM.pick (listOf arbitrary)
    dummyPath  <- QCM.pick arbitrary
    item       <- QCM.run (makeItem store dummyPath)
    return DT.DockerTaskInput{ 
        DT.inputBindings = [DT.VolumeBinding{ DT.item = item, DT.mount = p } | p <- mountPaths] , 
        DT.argsVals = argsVals
    }

checkOneInput :: (DT.DockerTaskInput -> Bool) -> IO CS.ContentStore -> Property
checkOneInput testPred getStore = QCM.monadicIO $ testPred <$> buildDockerTaskInput getStore

prop_DockerTaskInputMonoidLeftIdentity :: IO CS.ContentStore -> Property
prop_DockerTaskInputMonoidLeftIdentity = checkOneInput (\dti -> mempty <> dti == dti)

prop_DockerTaskInputMonoidRightIdentity :: IO CS.ContentStore -> Property
prop_DockerTaskInputMonoidRightIdentity = checkOneInput (\dti -> dti == dti <> mempty)

prop_DockerTaskInputMonoidAssociativity :: IO CS.ContentStore -> Property
prop_DockerTaskInputMonoidAssociativity getStore = QCM.monadicIO $ do
    x <- buildDockerTaskInput getStore
    y <- buildDockerTaskInput getStore
    z <- buildDockerTaskInput getStore
    return ((x <> y) <> z == x <> (y <> z))

{-monoidDockerTaskInputTests :: IO CS.ContentStore -> TestTree
monoidDockerTaskInputTests getStore = 
    testGroup "Monoid DockerInputTask tests" [
        do 
    ]
-}

monoidDockerTaskInputTests :: IO CS.ContentStore -> TestTree
monoidDockerTaskInputTests getStore = 
    testProperties "Monoid DockerInputTask tests" [(n, p getStore) | (n, p) <- tests]
    where tests = [ ("left identity", prop_DockerTaskInputMonoidLeftIdentity), 
                    ("right identity", prop_DockerTaskInputMonoidRightIdentity), 
                    ("associativity", prop_DockerTaskInputMonoidAssociativity)
                  ]

main :: IO ()
main = defaultMain $ testGroup "Units" [withResource buildStore CS.close monoidDockerTaskInputTests]
       --do
    {-quickCheck prop_DockerTaskInputMonoidLeftIdentity
    quickCheck prop_DockerTaskInputMonoidRightIdentity
    quickCheck prop_DockerTaskInputMonoidAssociativity-}
    --putStrLn "done!"
