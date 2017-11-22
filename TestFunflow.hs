{-# LANGUAGE Arrows            #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow
import           Control.Arrow.Free
import           Control.FunFlow.Base
import           Control.FunFlow.ContentHashable             (ContentHash)
import qualified Control.FunFlow.ContentStore                as CS
import           Control.FunFlow.Exec.Redis
import           Control.FunFlow.Exec.Simple
import           Control.FunFlow.External
import           Control.FunFlow.External.Coordinator.Memory
import           Control.FunFlow.External.Coordinator.Redis
import           Control.FunFlow.Pretty
import           Control.FunFlow.Steps
import           Control.Monad.Catch                         (Exception,
                                                              SomeException,
                                                              toException)
import           Data.Monoid                                 ((<>))
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
main = do
  memHook <- createMemoryCoordinator
  storeDir <- mkdtemp "test_output"
  res <- runSimpleFlow MemoryCoordinator memHook storeDir flow2 ()
  print res
  res' <- runSimpleFlow MemoryCoordinator memHook storeDir flow2caught ()
  print res'
  putStrLn $ showFlow myFlow
  putStrLn $ showFlow flow2
  res1 <- runSimpleFlow MemoryCoordinator memHook storeDir flow3 [1..10]
  print res1
-- main = redisTest
  externalTest
  storeTest

externalTest :: IO ()
externalTest = let
    someString = "External test"
    exFlow = external $ \t -> ExternalTask
      { _etCommand = "/run/current-system/sw/bin/echo"
      , _etParams = [textParam t]
      , _etWriteToStdOut = True
      }
    flow = exFlow >>> getFromStore (\d -> readFile $ d </> "out")
  in do
    storeDir <- mkdtemp "test_output_external_"
    withSimpleLocalRunner storeDir $ \run -> do
      out <- run flow someString
      case out of
        Left err     -> print err
        Right outStr -> putStrLn outStr

storeTest :: IO ()
storeTest = let
    string1 = "First line\n"
    string2 = "Second line\n"
    exFlow = external $ \(a, b) -> ExternalTask
      { _etCommand = "/run/current-system/sw/bin/cat"
      , _etParams = [pathParam a <> "/out", pathParam b <> "/out"]
      , _etWriteToStdOut = True
      }
    flow = proc (s1, s2) -> do
      f1 <- putInStore (\d s -> writeFile (d </> "out") s) -< s1
      s1' <- getFromStore (\d -> readFile $ d </> "out") -< f1
      f2 <- putInStore (\d s -> writeFile (d </> "out") s) -< s2
      s2' <- getFromStore (\d -> readFile $ d </> "out") -< f2
      f12 <- exFlow -< (f1, f2)
      s12 <- getFromStore (\d -> readFile $ d </> "out") -< f12
      returnA -< s12 == s1' <> s2'
  in do
    storeDir <- mkdtemp "test_output_store_"
    withSimpleLocalRunner storeDir $ \run -> do
      out <- run flow (string1, string2)
      case out of
        Left err -> print err
        Right b  -> print b

redisTest :: IO ()
redisTest = let
    redisConf = R.defaultConnectInfo {
        R.connectHost = "10.233.2.2"
      , R.connectPort = R.PortNumber . fromIntegral $ 6379
      , R.connectAuth = Nothing
      }
    someString = "Hello World" :: T.Text
    flow :: SimpleFlow T.Text CS.Item
    flow = external $ \t -> ExternalTask {
        _etCommand = "/run/current-system/sw/bin/echo"
      , _etParams = [textParam t]
      , _etWriteToStdOut = True
      }
  in do
    storeDir <- mkdtemp "test_output"
    out <- runSimpleFlow Redis redisConf storeDir flow someString
    print out
