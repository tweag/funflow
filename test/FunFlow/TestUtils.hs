{-# LANGUAGE GADTs                     #-}

module FunFlow.TestUtils where

import           Control.FunFlow.Base
import           Control.FunFlow.Steps

data FlowInvariant where
  FlowInvariant :: a -> Flow SomeException a b -> Maybe b -> FlowInvariant

flowInvariants :: [FlowInvariant]
flowInvariants =
  [ FlowInvariant "foo" melancholicLazarus Nothing
  , FlowInvariant "bar" (retry 1 1 melancholicLazarus) (Just "bar")
  ]