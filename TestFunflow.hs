{-# LANGUAGE Arrows, GADTs, OverloadedStrings #-}

import Control.FunFlow.Base
import Control.FunFlow.Steps
import Control.FunFlow.Pretty
import Control.FunFlow.Exec.Local
import Control.FunFlow.Exec.Simple
import Control.FunFlow.Exec.Redis
import Control.Arrow
import Database.Redis
import Control.Monad.IO.Class

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

allJobs = [("job1", flow2)]

main :: IO ()
main = do conn <- connect defaultConnectInfo
          runRFlow conn $ do jid <- sparkJob "job1" ()
                             resumeFirstJob allJobs
                             js <- getJobStatus jid
                             liftIO $ print js
          return ()

          {-res <- runTillDone flow2 ()
          print res
          putStrLn $ showFlow myFlow
          putStrLn $ showFlow flow2
          res1 <- runFlow flow3 [1..10]
          print res1 -}
