{-# LANGUAGE Arrows, GADTs, OverloadedStrings #-}

import Control.FunFlow.Base
import Control.FunFlow.Steps
import Control.FunFlow
import Control.Arrow


myFlow :: Flow () Bool
myFlow = proc () -> do
  age <- promptFor <: "getNm" -< "How old are you"
  returnA -< age > (65::Int)

flow2 :: Flow () (Double,Double)
flow2 = proc () -> do
  r1 <- worstBernoulli -< 0.1
  r2 <- worstBernoulli -< 0.2
  returnA -< (r1,r2)

main :: IO ()
main = do res <- runTillDone flow2 ()
          print res