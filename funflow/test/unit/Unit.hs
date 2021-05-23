{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- Funflow unit tests

import Data.Maybe (fromMaybe)
import Path (Abs, Dir, Path, parseAbsDir, toFilePath)
import Path.IO (createDirIfMissing)
import System.Directory (getCurrentDirectory)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperties)
import qualified Test.QuickCheck.Monadic as QCM

import qualified Data.CAS.ContentHashable as CH
import qualified Data.CAS.ContentStore as CS
import qualified Data.CAS.RemoteCache as RC
import qualified Funflow.Tasks.Docker as DT
import TInstances()

myNoOp :: Path Abs Dir -> CH.DirectoryContent -> IO ()
myNoOp _ _ = return ()

myNoErr :: CH.ContentHash -> IO ()
myNoErr _ = error "uh-oh!"

type Fixture = IO CS.Item

fixture :: Fixture
fixture = tmp >>= (\p -> CS.withStore p (\s -> mkIt s p))
    where path = getCurrentDirectory >>= parseAbsDir >>= (\p -> parseAbsDir $ toFilePath p ++ ".tmp/store")
          tmp = path >>= createDirIfMissing True >>= (\_ -> path)
          mkIt s p = CS.putInStore s RC.NoCache myNoErr myNoOp (CH.DirectoryContent p)

buildDockerTaskInput :: Fixture -> QCM.PropertyM IO DT.DockerTaskInput
buildDockerTaskInput setup = do
    item         <- QCM.run setup
    argsVals     <- QCM.pick arbitrary
    mountPaths   <- QCM.pick (listOf arbitrary)
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
    let makeProp x prop = withMaxSuccess (fromMaybe 100 x) (prop setup)
        specs = [ ("left identity", prop_DockerTaskInputMonoidLeftIdentity, Nothing), 
                  ("right identity", prop_DockerTaskInputMonoidRightIdentity, Nothing), 
                  ("associativity", prop_DockerTaskInputMonoidAssociativity, Nothing)
                ]
    in testProperties "Monoid DockerInputTask tests" [(n, makeProp x p) | (n, p, x) <- specs]

main :: IO ()
main = let cleanup _ = return ()
           testCtx   = withResource fixture cleanup
       in defaultMain $ testGroup "Units" [testCtx monoidDockerTaskInputTests]
