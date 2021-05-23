-- Funflow unit tests

import Data.List (permutations)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Set as Set
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
import TUtils (uniqueGen, unsafeToAbsDir)

myNoOp :: Path Abs Dir -> CH.DirectoryContent -> IO ()
myNoOp _ _ = return ()

myNoErr :: CH.ContentHash -> IO ()
myNoErr _ = error "uh-oh!"

type Fixture = IO CS.Item
type TaskArgVals = Map.Map String Text.Text
type TaskInput = DT.DockerTaskInput

fixture :: Fixture
fixture = tmp >>= (\p -> CS.withStore p (\s -> mkIt s p))
    where path = getCurrentDirectory >>= parseAbsDir >>= (\p -> parseAbsDir $ toFilePath p ++ ".tmp/store")
          tmp = path >>= createDirIfMissing True >>= (\_ -> path)
          mkIt s p = CS.putInStore s RC.NoCache myNoErr myNoOp (CH.DirectoryContent p)

makeTask :: CS.Item -> Mounts -> TaskArgVals -> TaskInput
makeTask it mounts argValLookup = 
        let makeBind p = DT.VolumeBinding{ DT.item = it, DT.mount = p }
        in DT.DockerTaskInput{ DT.inputBindings = map makeBind mounts, DT.argsVals = argValLookup }

buildDockerTaskInput :: Gen Mounts -> Fixture -> QCM.PropertyM IO TaskInput
buildDockerTaskInput genMounts setup = do
    item         <- QCM.run setup
    argsVals     <- QCM.pick arbitrary
    mountPaths   <- QCM.pick genMounts
    return (makeTask item mountPaths argsVals)

type Mounts = [Path Abs Dir]

---------------------------------------------------------------------------------------------
-- overlapping mounts generation
---------------------------------------------------------------------------------------------
genOverlappedMounts :: Gen (Mounts, Mounts)
genOverlappedMounts = 
    let pool   = permutations "abc"
        gen k = vectorOf k (unsafeToAbsDir <$> elements pool)
        lo   = 1 + floor (fromIntegral (length pool) / 2)
    in sized $ \n -> do
        let hi = max lo n
        mnts1 <- choose (lo, hi) >>= gen
        mnts2 <- choose (lo, hi) >>= gen
        return (mnts1, mnts2)

finalizeInputsWithOverlappingMounts :: CS.Item 
                                    -> (TaskArgVals, TaskArgVals)
                                    -> Gen (TaskInput, TaskInput)
finalizeInputsWithOverlappingMounts item (lookup1, lookup2) = 
    let build (mnts1, mnts2) = (makeTask item mnts1 lookup1, makeTask item mnts2 lookup2)
    in build <$> genOverlappedMounts

buildInputsWithOverlappingMounts :: Fixture -> QCM.PropertyM IO (TaskInput, TaskInput)
buildInputsWithOverlappingMounts setup = do
    item    <- QCM.run setup
    lookup1 <- QCM.pick arbitrary
    lookup2 <- QCM.pick arbitrary
    QCM.pick $ finalizeInputsWithOverlappingMounts item (lookup1, lookup2)
---------------------------------------------------------------------------------------------


buildDockerTaskInputUniqueMounts :: Fixture -> QCM.PropertyM IO TaskInput
buildDockerTaskInputUniqueMounts = buildDockerTaskInput (uniqueGen (listOf arbitrary))

genMountsWithMin1Repeat :: Gen Mounts
genMountsWithMin1Repeat = 
    let pool   = permutations "abc"
        genDir = unsafeToAbsDir <$> elements pool
    in sized $ \n -> vectorOf (n `max` 1 + length pool) genDir

buildDockerTaskInputWithRepeatMounts :: Fixture -> QCM.PropertyM IO TaskInput
buildDockerTaskInputWithRepeatMounts = buildDockerTaskInput genMountsWithMin1Repeat

getMounts :: TaskInput -> Mounts
getMounts = map DT.mount . DT.inputBindings

uniqueMounts :: TaskInput -> Set.Set (Path Abs Dir)
uniqueMounts = Set.fromList . getMounts

mountsMatchUptoSet :: TaskInput -> TaskInput -> Bool
mountsMatchUptoSet x y = uniqueMounts x == uniqueMounts y

nonzeroMountReductionIsCorrect :: TaskInput -> TaskInput -> Bool
nonzeroMountReductionIsCorrect old new = 
    let numOld = length (getMounts old) 
        numRep = numOld - length (uniqueMounts old)
    in numRep > 0 && numOld - numRep == length (uniqueMounts new)

checkMountDeduplication :: (TaskInput -> TaskInput) -> Fixture -> Property
checkMountDeduplication build setup = 
    let validate x y = mountsMatchUptoSet x y && nonzeroMountReductionIsCorrect x y
    in QCM.monadicIO $ (\x -> let y = build x in validate x y) <$> buildDockerTaskInputWithRepeatMounts setup

checkOneInput :: (TaskInput -> Bool) -> Fixture -> Property
checkOneInput testPred setup = QCM.monadicIO $ testPred <$> buildDockerTaskInputUniqueMounts setup

prop_DockerTaskInputMonoidLeftIdentity :: Fixture -> Property
prop_DockerTaskInputMonoidLeftIdentity = checkOneInput (\dti -> mempty <> dti == dti)

prop_DockerTaskInputMonoidRightIdentity :: Fixture -> Property
prop_DockerTaskInputMonoidRightIdentity = checkOneInput (\dti -> dti == dti <> mempty)

prop_DockerTaskInputMonoidAssociativity :: Fixture -> Property
prop_DockerTaskInputMonoidAssociativity setup = QCM.monadicIO $ do
    x <- buildDockerTaskInputUniqueMounts setup
    y <- buildDockerTaskInputUniqueMounts setup
    z <- buildDockerTaskInputUniqueMounts setup
    return ( (x <> y) <> z == x <> (y <> z) )

prop_DockerTaskInputLeftIdentityDeduplicatesMounts :: Fixture -> Property
prop_DockerTaskInputLeftIdentityDeduplicatesMounts = checkMountDeduplication (mempty <>)

prop_DockerTaskInputRightIdentityDeduplicatesMounts :: Fixture -> Property
prop_DockerTaskInputRightIdentityDeduplicatesMounts = checkMountDeduplication (<> mempty)

prop_DockerTaskInputCombineDeduplicatesMounts :: Fixture -> Property
prop_DockerTaskInputCombineDeduplicatesMounts setup = 
    let nontrivial = not . Set.null . uncurry Set.intersection
    in QCM.monadicIO $ do
        (in1, in2) <- buildInputsWithOverlappingMounts setup
        let uniqs@(uniq1, uniq2) = (uniqueMounts in1, uniqueMounts in2)
            obs = getMounts (in1 <> in2)
            expUniq = Set.union uniq1 uniq2
        return ( nontrivial uniqs && length obs == length expUniq && Set.fromList obs == expUniq )

monoidDockerTaskInputTests :: Fixture -> TestTree
monoidDockerTaskInputTests setup = 
    let makeProp x prop = withMaxSuccess (fromMaybe 100 x) (prop setup)
        specs = [ ("left identity pure", prop_DockerTaskInputMonoidLeftIdentity, Nothing), 
                  ("right identity pure", prop_DockerTaskInputMonoidRightIdentity, Nothing), 
                  ("associativity pure", prop_DockerTaskInputMonoidAssociativity, Nothing), 
                  ("left identity dedup", prop_DockerTaskInputLeftIdentityDeduplicatesMounts, Nothing), 
                  ("right identity dedup", prop_DockerTaskInputRightIdentityDeduplicatesMounts, Nothing), 
                  ("combine dedup", prop_DockerTaskInputCombineDeduplicatesMounts, Nothing)
                ]
    in testProperties "Monoid DockerInputTask tests" [(n, makeProp x p) | (n, p, x) <- specs]

main :: IO ()
main = let cleanup _ = return ()
           testCtx   = withResource fixture cleanup
       in defaultMain $ testGroup "Units" [testCtx monoidDockerTaskInputTests]
