{-# LANGUAGE Arrows, GADTs, OverloadedStrings #-}

import Control.FunFlow.Base
import Control.FunFlow.Steps
import Control.FunFlow.Pretty
import Control.FunFlow.Exec.Local
import Control.FunFlow.Exec.Simple
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

flow3 :: Flow [Int] [Int]
flow3 = mapF (arr (+1))

main :: IO ()
main = do res <- runTillDone flow2 ()
          print res
          putStrLn $ showFlow myFlow
          putStrLn $ showFlow flow2
          res1 <- runFlow flow3 [1..10]
          print res1
