{-# LANGUAGE Arrows            #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow
import           Control.Arrow.Free
import           Control.FunFlow.Base
import           Control.FunFlow.ContentHashable             (ContentHash,
                                                              hashToPath)
import qualified Control.FunFlow.ContentStore                as CS
import           Control.FunFlow.Exec.Local
import           Control.FunFlow.Exec.Redis
import           Control.FunFlow.Exec.Simple
import           Control.FunFlow.External
import           Control.FunFlow.External.Coordinator.Memory
import qualified Control.FunFlow.External.Docker             as Docker
import           Control.FunFlow.Pretty
import           Control.FunFlow.Steps
import           Control.Monad.Catch                         (Exception,
                                                              SomeException,
                                                              toException)
import           Data.Semigroup
import qualified Data.Text                                   as T
import qualified Database.Redis                              as R
import           System.FilePath                             ((</>))
import           System.Posix.Temp                           (mkdtemp)

mkError :: String -> SomeException
mkError = toException . userError

myFlow :: SimpleFlow () Bool
myFlow = proc () -> do
  age <- promptFor -< "How old are you"
  returnA -< age > (65::Int)

flow2 :: SimpleFlow () (Double,Double)
flow2 = proc () -> do
  r1 <- worstBernoulli mkError -< 0.1
  r2 <- worstBernoulli mkError -< 0.2
  returnA -< (r1,r2)

flow2caught :: SimpleFlow () (Double,Double)
flow2caught = retry 100 0 flow2

flow3 :: SimpleFlow [Int] [Int]
flow3 = mapA (arr (+1))

allJobs = [("job1", flow2)]

main :: IO ()
-- main = do
--   storeDir <- mkdtemp "test"
--   res <- runSimpleFlow MemoryCoordinator () storeDir flow2 ()
--   print res
--   res' <- runSimpleFlow MemoryCoordinator () storeDir flow2caught ()
--   print res'
--   putStrLn $ showFlow myFlow
--   putStrLn $ showFlow flow2
--   res1 <- runSimpleFlow MemoryCoordinator () storeDir flow3 [1..10]
--   print res1
-- XXX: First run
-- $ stack exec ffexecutord -- --storePath ./store --redisHost 127.0.0.1 --redisPort 6379
main = redisTest

echoFlow :: SimpleFlow T.Text ContentHash
echoFlow = external $ \t -> ExternalTask
  { _etCommand = "/run/current-system/sw/bin/echo"
  , _etParams = [textParam t]
  , _etWriteToStdOut = True
  }

catFlow :: SimpleFlow ContentHash ContentHash
catFlow = external $ \h -> ExternalTask
  { _etCommand = "/run/current-system/sw/bin/cat"
  , _etParams = [pathParam h <> "/out"]
  , _etWriteToStdOut = True
  }

cpFlow :: SimpleFlow ContentHash ContentHash
cpFlow = external $ \h -> ExternalTask
  { _etCommand = "/run/current-system/sw/bin/cp"
  , _etParams = ["-r", pathParam h, textParam "./"]
  , _etWriteToStdOut = False
  }

dockerFlow :: SimpleFlow ContentHash ContentHash
dockerFlow = docker $ \input -> Docker.Config
  { Docker.image = "busybox"
  , Docker.optImageID = Just "6ad733544a63"
  , Docker.input = Docker.SingleInput input
  , Docker.command = "sh"
  , Docker.args = ["-c", "cat /input/out /input/out > /output/out"]
  }

redisTest :: IO ()
redisTest = let
    redisConf = R.defaultConnectInfo {
        R.connectHost = "127.0.0.1"
      , R.connectPort = R.PortNumber . fromIntegral $ 6379
      , R.connectAuth = Nothing
      }
    someString = "Hello World" :: T.Text
    flow :: SimpleFlow T.Text ContentHash
    flow = proc t -> do
      r <- echoFlow -< t
      r' <- catFlow -< r
      dockerFlow -< r'
  in do
    --storeDir <- mkdtemp "test"
    let storeDir = "store"
    out <- runSimpleFlow Redis redisConf storeDir flow someString
    print $ hashToPath <$> out
