{-# LANGUAGE Arrows            #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow
import           Control.Arrow.Free
import           Control.FunFlow.Base
import           Control.FunFlow.Exec.Local
import           Control.FunFlow.Exec.Redis
import           Control.FunFlow.Exec.Simple
import           Control.FunFlow.External.Coordinator.Memory
import           Control.FunFlow.Pretty
import           Control.FunFlow.Steps
import           Control.Monad.Catch                         (Exception)
import           Database.Redis

newtype MyEx = MyEx [Char]
  deriving Show
instance Exception MyEx

myFlow :: Flow MyEx () Bool
myFlow = proc () -> do
  age <- promptFor -< "How old are you"
  returnA -< age > (65::Int)

flow2 :: Flow MyEx () (Double,Double)
flow2 = proc () -> do
  r1 <- worstBernoulli MyEx -< 0.1
  r2 <- worstBernoulli MyEx -< 0.2
  returnA -< (r1,r2)

flow3 :: Flow MyEx [Int] [Int]
flow3 = mapA (arr (+1))

allJobs = [("job1", flow2)]

main :: IO ()
main = do res <- runTillDone flow2 ()
          print res
          putStrLn $ showFlow myFlow
          putStrLn $ showFlow flow2
          res1 <- runFlow MemoryCoordinator () flow3 [1..10]
          print res1
