-- Funflow unit tests

import qualified Data.CAS.ContentHashable as CH
import qualified Data.CAS.ContentStore as CS
import qualified Data.CAS.RemoteCache as RC
import Data.List (permutations)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Funflow.Tasks.Docker as DT
import Path (Abs, Dir, Path, parseAbsDir, toFilePath)
import Path.IO (createDirIfMissing)
import System.Directory (getCurrentDirectory)
import TInstances ()
import TUtils (uniqueGen, unsafeToAbsDir)
import Test.QuickCheck
import qualified Test.QuickCheck.Monadic as QCM
import Test.Tasty
import Test.Tasty.QuickCheck (testProperties)

type Fixture = IO CS.Item

type Mounts = [Path Abs Dir]

type TaskArgVals = Map.Map String Text.Text

type TaskInput = DT.DockerTaskInput

-- do-nothing action for generating a CS.Item
myNoOp :: Path Abs Dir -> CH.DirectoryContent -> IO ()
myNoOp _ _ = return ()

-- do-nothing action for error handling when making CS.Item
myNoErr :: CH.ContentHash -> IO ()
myNoErr _ = error "uh-oh!"

-- provide test cases with a CS.Item
fixture :: Fixture
fixture = tmp >>= (\p -> CS.withStore p (\s -> mkIt s p))
  where
    path = getCurrentDirectory >>= parseAbsDir >>= (\p -> parseAbsDir $ toFilePath p ++ ".tmp/store")
    tmp = path >>= createDirIfMissing True >>= (\_ -> path)
    mkIt s p = CS.putInStore s RC.NoCache myNoErr myNoOp (CH.DirectoryContent p)

-- convenience helper
makeTask :: CS.Item -> Mounts -> TaskArgVals -> TaskInput
makeTask it mounts argValLookup =
  let makeBind p = DT.VolumeBinding {DT.item = it, DT.mount = p}
   in DT.DockerTaskInput {DT.inputBindings = map makeBind mounts, DT.argsVals = argValLookup}

-- Extract the mounts from a task input.
getMounts :: TaskInput -> Mounts
getMounts = map DT.mount . DT.inputBindings

-- Extract and deduplicate mounts from a task input.
uniqueMounts :: TaskInput -> Set.Set (Path Abs Dir)
uniqueMounts = Set.fromList . getMounts

---------------------------------------------------------------------------------------------
-- overlapping mounts generation
---------------------------------------------------------------------------------------------
buildInputsWithOverlappingMounts :: Fixture -> QCM.PropertyM IO (TaskInput, TaskInput)
buildInputsWithOverlappingMounts setup =
  let pool = permutations "abc"
      gen k = vectorOf k (unsafeToAbsDir <$> elements pool)
      lo = 1 + floor (fromIntegral (length pool) / 2)
      listsIsect (x, y) = not . null $ Set.intersection (Set.fromList x) (Set.fromList y)
      genCandidate = sized $ \n -> do
        let hi = max lo n
        mnts1 <- choose (lo, hi) >>= gen
        mnts2 <- choose (lo, hi) >>= gen
        return (mnts1, mnts2)
      genOverlappedMounts = genCandidate `suchThat` listsIsect
      finalizeInputsWithOverlappingMounts item argMap1 argMap2 =
        (\(ms1, ms2) -> (makeTask item ms1 argMap1, makeTask item ms2 argMap2)) <$> genOverlappedMounts
   in do
        item <- QCM.run setup
        lookup1 <- QCM.pick arbitrary
        lookup2 <- QCM.pick arbitrary
        QCM.pick $ finalizeInputsWithOverlappingMounts item lookup1 lookup2

---------------------------------------------------------------------------------------------

-- given a mounts generation strategy and (Item) fixture, generate a task input value.
buildDockerTaskInput :: Gen Mounts -> Fixture -> QCM.PropertyM IO TaskInput
buildDockerTaskInput genMounts setup = do
  item <- QCM.run setup
  argsVals <- QCM.pick arbitrary
  mountPaths <- QCM.pick genMounts
  return (makeTask item mountPaths argsVals)

-- Create a task input for which each mount point is unique.
buildDockerTaskInputUniqueMounts :: Fixture -> QCM.PropertyM IO TaskInput
buildDockerTaskInputUniqueMounts = buildDockerTaskInput (uniqueGen (listOf arbitrary))

-- Create a task input for which mount points have at least 1 repeat.
buildDockerTaskInputWithRepeatMounts :: Fixture -> QCM.PropertyM IO TaskInput
buildDockerTaskInputWithRepeatMounts =
  let pool = permutations "abc"
      genDir = unsafeToAbsDir <$> elements pool
      genMountsWithMin1Repeat = sized $ \n -> vectorOf (n `max` 1 + length pool) genDir
   in buildDockerTaskInput genMountsWithMin1Repeat

-- Test that mounts become unique after doing some operation to a task input.
checkMountDeduplication :: (TaskInput -> TaskInput) -> Fixture -> Property
checkMountDeduplication build setup =
  let mountsMatchUptoSet x y = uniqueMounts x == uniqueMounts y
      validate x y = mountsMatchUptoSet x y && nonzeroMountReductionIsCorrect x y
      nonzeroMountReductionIsCorrect old new = numRep > 0 && numOld - numRep == length (uniqueMounts new)
        where
          numOld = length (getMounts old)
          numRep = numOld - length (uniqueMounts old)
   in QCM.monadicIO $ (\x -> let y = build x in validate x y) <$> buildDockerTaskInputWithRepeatMounts setup

-- Create a property from a predicate and a fixture, using unique mount points for task input.
checkOneInputUniqueMounts :: (TaskInput -> Bool) -> Fixture -> Property
checkOneInputUniqueMounts testPred setup = QCM.monadicIO $ testPred <$> buildDockerTaskInputUniqueMounts setup

-- When mounts are unique, left identity should hold for task input monoid.
prop_DockerTaskInputMonoidLeftIdentity :: Fixture -> Property
prop_DockerTaskInputMonoidLeftIdentity = checkOneInputUniqueMounts (\dti -> mempty <> dti == dti)

-- When mounts are unique, right identity should hold for task input monoid.
prop_DockerTaskInputMonoidRightIdentity :: Fixture -> Property
prop_DockerTaskInputMonoidRightIdentity = checkOneInputUniqueMounts (\dti -> dti == dti <> mempty)

-- When mounts are unique, right identity should hold for task input semigroup.
prop_DockerTaskInputMonoidAssociativity :: Fixture -> Property
prop_DockerTaskInputMonoidAssociativity setup = QCM.monadicIO $ do
  x <- buildDockerTaskInputUniqueMounts setup
  y <- buildDockerTaskInputUniqueMounts setup
  z <- buildDockerTaskInputUniqueMounts setup
  return ((x <> y) <> z == x <> (y <> z))

-- When mounts are not unique, left identity should deduplicate them.
prop_DockerTaskInputLeftIdentityDeduplicatesMounts :: Fixture -> Property
prop_DockerTaskInputLeftIdentityDeduplicatesMounts = checkMountDeduplication (mempty <>)

-- When mounts are not unique, right identity should deduplicate them.
prop_DockerTaskInputRightIdentityDeduplicatesMounts :: Fixture -> Property
prop_DockerTaskInputRightIdentityDeduplicatesMounts = checkMountDeduplication (<> mempty)

-- When mounts overlap between two task inputs, semigroup combine should deduplicate them.
prop_DockerTaskInputCombineDeduplicatesMounts :: Fixture -> Property
prop_DockerTaskInputCombineDeduplicatesMounts setup =
  let nontrivial = not . Set.null . uncurry Set.intersection
   in QCM.monadicIO $ do
        (in1, in2) <- buildInputsWithOverlappingMounts setup
        let uniqs@(uniq1, uniq2) = (uniqueMounts in1, uniqueMounts in2)
            obs = getMounts (in1 <> in2)
            expUniq = Set.union uniq1 uniq2
        return (nontrivial uniqs && length obs == length expUniq && Set.fromList obs == expUniq)

-- all Monoid tests for task input
monoidDockerTaskInputTests :: Fixture -> TestTree
monoidDockerTaskInputTests setup =
  let makeProp x prop = withMaxSuccess (fromMaybe 100 x) (prop setup)
      specs =
        [ ("left identity pure", prop_DockerTaskInputMonoidLeftIdentity, Nothing),
          ("right identity pure", prop_DockerTaskInputMonoidRightIdentity, Nothing),
          ("associativity pure", prop_DockerTaskInputMonoidAssociativity, Nothing),
          ("left identity dedup", prop_DockerTaskInputLeftIdentityDeduplicatesMounts, Nothing),
          ("right identity dedup", prop_DockerTaskInputRightIdentityDeduplicatesMounts, Nothing),
          ("combine dedup", prop_DockerTaskInputCombineDeduplicatesMounts, Just 1000)
        ]
   in testProperties "Monoid DockerInputTask tests" [(n, makeProp x p) | (n, p, x) <- specs]

-- Run tests.
main :: IO ()
main =
  let cleanup _ = return ()
      testCtx = withResource fixture cleanup
   in defaultMain $ testGroup "Units" [testCtx monoidDockerTaskInputTests]
