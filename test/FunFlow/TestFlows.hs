{-# LANGUAGE GADTs                     #-}

module FunFlow.TestFlows where

import           Control.FunFlow.Base
import           Control.FunFlow.Steps
import           Test.Tasty
import           Test.Tasty.HUnit
import           Control.Exception
import           System.Directory
import           Control.Monad (when)

import           Control.FunFlow.Exec.Local
import           Control.FunFlow.Exec.Redis
import           Control.FunFlow.Exec.Simple
import           Control.FunFlow.External.Coordinator.Memory

data FlowInvariant where
  FlowInvariant :: (Eq b, Show b)
                => String -- test name
                -> a  -- input
                -> Flow SomeException a b -- the flow to test
                -> Maybe b --expected output - Nothing for expected failure
                -> IO () -- test setup action
                -> FlowInvariant

flowInvariants :: [FlowInvariant]
flowInvariants =
  [ FlowInvariant "death" "foo" melancholicLazarus Nothing setup
  , FlowInvariant "resurrection" "bar" (retry 1 1 melancholicLazarus) (Just "bar") setup
  ]

setup :: IO ()
setup = do ex <- doesFileExist "/tmp/lazarus_note"
           when ex $ removeFile "/tmp/lazarus_note"

testFlowInvariant :: FlowInvariant -> TestTree
testFlowInvariant (FlowInvariant nm x flw expect before) =
  testCase nm $ do
    before
    res <- runFlow MemoryCoordinator () flw x
    case (expect, res) of
      (Nothing, Left _) -> return ()
      (Just x, Right y) -> assertEqual "flow results" x y
      (Nothing, Right y) -> assertFailure $ "expected flow failure, got success" ++ show y
      (Just x, Left err) -> assertFailure $ "expected success "++ show x++", got error" ++ show err

tests :: TestTree
tests = testGroup "Flow invariants" $ map testFlowInvariant flowInvariants