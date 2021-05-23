{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

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
import Path (Abs, Dir, Path, mkRelDir, parseAbsDir, toFilePath, (</>))
import Path.IO (createDirIfMissing)
import Test.Tasty
import Test.Tasty.QuickCheck (testProperties)
import qualified Data.CAS.RemoteCache as RC
import System.Directory (getCurrentDirectory, removeDirectoryRecursive)
import TUtils (randomRel1)

myNoOp :: Path Abs Dir -> CH.DirectoryContent -> IO ()
myNoOp _ _ = return ()

myNoErr :: CH.ContentHash -> IO ()
myNoErr _ = error "uh-oh!"

type Fixture = IO (Path Abs Dir, CS.ContentStore)

fixture :: Fixture
{-fixture = currTmp >>= (\p -> (\s -> (p,s)) <$> CS.open p)
    where currAbs = toFilePath <$> (getCurrentDirectory >>= parseAbsDir)
          currTmp = currAbs >>= (\d -> parseAbsDir (d ++ "tmp/store"))
-}
{-fixture = parseAbsDir d >>= (\p -> (\s -> (p,s)) <$> CS.open p)
    where d = "/home/vr/sandbox/" ++ "tmp/store"
-}
{-fixture = d >>= parseAbsDir >>= (\p -> (\s -> (p,s)) <$> CS.open p)
    where d = (++ "./tmp/store") . toFilePath <$> getCurrentDirectory >>= parseAbsDir
-}
fixture = tmp >>= (\p -> (\s -> (p,s)) <$> CS.open p)
    where path = getCurrentDirectory >>= parseAbsDir >>= (\p -> parseAbsDir $ toFilePath p ++ ".tmp/store")
          tmp = path >>= createDirIfMissing True >>= (\_ -> path)

makeItem :: CS.ContentStore -> Path Abs Dir -> IO CS.Item
makeItem store p = CS.putInStore store RC.NoCache myNoErr myNoOp (CH.DirectoryContent p)

buildDockerTaskInput :: Fixture -> QCM.PropertyM IO DT.DockerTaskInput
buildDockerTaskInput setup = do
    (tmp, store) <- QCM.run setup
    argsVals     <- QCM.pick arbitrary
    mountPaths   <- QCM.pick (listOf arbitrary)
    rel2tmp      <- QCM.pick randomRel1
    --target       <- QCM.run (parseAbsDir (toFilePath tmp ++ rel2tmp))
    --item         <- QCM.run (makeItem store target)
    item         <- QCM.run (makeItem store tmp)
    return DT.DockerTaskInput{ 
        DT.inputBindings = [DT.VolumeBinding{ DT.item = item, DT.mount = p } | p <- mountPaths] , 
        DT.argsVals = argsVals
    }

checkOneInput :: (DT.DockerTaskInput -> Bool) -> Fixture -> Property
checkOneInput testPred setup = QCM.monadicIO $ testPred <$> buildDockerTaskInput setup

prop_DockerTaskInputMonoidLeftIdentity :: Fixture -> Property
prop_DockerTaskInputMonoidLeftIdentity = checkOneInput (\dti -> mempty <> dti == dti)

prop_DockerTaskInputMonoidRightIdentity :: Fixture -> Property
prop_DockerTaskInputMonoidRightIdentity = checkOneInput (\dti -> dti == dti <> mempty)

prop_DockerTaskInputMonoidAssociativity :: Fixture -> Property
prop_DockerTaskInputMonoidAssociativity setup = QCM.monadicIO $ do
    x <- buildDockerTaskInput setup
    y <- buildDockerTaskInput setup
    z <- buildDockerTaskInput setup
    return ((x <> y) <> z == x <> (y <> z))

monoidDockerTaskInputTests :: Fixture -> TestTree
monoidDockerTaskInputTests setup = 
    testProperties "Monoid DockerInputTask tests" [(n, p setup) | (n, p) <- tests]
    where tests = [ ("left identity", prop_DockerTaskInputMonoidLeftIdentity), 
                    ("right identity", prop_DockerTaskInputMonoidRightIdentity), 
                    ("associativity", prop_DockerTaskInputMonoidAssociativity)
                  ]

main :: IO ()
--main = let cleanup (tmpdir, store) = CS.close store >> removeDirectoryRecursive (toFilePath tmpdir)
main = let cleanup (_, store) = CS.close store
           testCtx            = withResource fixture cleanup
       in defaultMain $ testGroup "Units" [testCtx monoidDockerTaskInputTests]
       --do
    {-quickCheck prop_DockerTaskInputMonoidLeftIdentity
    quickCheck prop_DockerTaskInputMonoidRightIdentity
    quickCheck prop_DockerTaskInputMonoidAssociativity-}
    --putStrLn "done!"
