{-# LANGUAGE GADTs #-}

module FunFlow.TestFlows where

import           Control.Arrow
import           Control.Exception
import           Control.FunFlow.Base
import           Control.FunFlow.Steps
import           Control.Monad                               (when)
import           Data.Dynamic
import           System.Directory
import           Test.Tasty
import           Test.Tasty.HUnit

import           Control.FunFlow.Exec.Local
import           Control.FunFlow.Exec.Redis
import           Control.FunFlow.Exec.Simple
import           Control.FunFlow.External.Coordinator.Memory

data FlowAssertion where
  FlowAssertion :: (Eq b, Show b)
                => String -- test name
                -> a  -- input
                -> Flow SomeException a b -- the flow to test
                -> Maybe b --expected output - Nothing for expected failure
                -> IO () -- test setup action
                -> FlowAssertion

flowAssertions :: [FlowAssertion]
flowAssertions =
  [ FlowAssertion "death" "foo" melancholicLazarus Nothing setup
  , FlowAssertion "resurrection" "bar" (retry 1 1 melancholicLazarus) (Just "bar") setup
  , FlowAssertion "bernoulli" 0.2 (retry 20 0 $ worstBernoulli (toException . toDyn) >>> arr (<2.0)) (Just True) (return ())
  , FlowAssertion "failStep" () failStep Nothing (return ())

  ]

setup :: IO ()
setup = do ex <- doesFileExist "/tmp/lazarus_note"
           when ex $ removeFile "/tmp/lazarus_note"

testFlowAssertion :: FlowAssertion -> TestTree
testFlowAssertion (FlowAssertion nm x flw expect before) =
  testCase nm $ do
    before
    res <- runFlow MemoryCoordinator () flw x
    case (expect, res) of
      (Nothing, Left _) -> return ()
      (Just x, Right y) -> assertEqual "flow results" x y
      (Nothing, Right y) -> assertFailure $ "expected flow failure, got success" ++ show y
      (Just x, Left err) -> assertFailure $ "expected success "++ show x++", got error" ++ show err

tests :: TestTree
tests = testGroup "Flow Assertions" $ map testFlowAssertion flowAssertions
