{-# LANGUAGE GADTs                     #-}

module FunFlow.TestFlows where

import           Control.FunFlow.Base
import           Control.FunFlow.Steps
import           Test.Tasty
import           Test.Tasty.HUnit
import           Control.Exception

import           Control.FunFlow.Exec.Local
import           Control.FunFlow.Exec.Redis
import           Control.FunFlow.Exec.Simple
import           Control.FunFlow.External.Coordinator.Memory

data FlowInvariant where
  FlowInvariant :: String -> a -> Flow SomeException a b -> Maybe b -> FlowInvariant

flowInvariants :: [FlowInvariant]
flowInvariants =
  [ FlowInvariant "death" "foo" melancholicLazarus Nothing
  , FlowInvariant "resurrection" "bar" (retry 1 1 melancholicLazarus) (Just "bar")
  ]

testFlowInvariant :: FlowInvariant -> TestTree
testFlowInvariant (FlowInvariant nm x flw expect) =
  testCase nm $ do
    res <- runFlow MemoryCoordinator () flw x
    case (expect, res) of
    return undefined

tests :: TestTree
tests = testGroup "Flow invariants" $ map testFlowInvariant flowInvariants