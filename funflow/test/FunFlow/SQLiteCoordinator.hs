{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module FunFlow.SQLiteCoordinator where

import           Control.Arrow
import           Control.Arrow.Free
import           Control.Exception
import           Control.FunFlow
import qualified Control.FunFlow.ContentStore                as CS
import           Control.FunFlow.Exec.Simple
import           Control.FunFlow.External.Coordinator.SQLite
import           Control.FunFlow.Steps
import           Control.Monad
import           Data.String                                 (fromString)
import           Path
import           Path.IO
import           System.Process
import           Test.Tasty
import           Test.Tasty.HUnit

storeDir :: Path Rel Dir
storeDir = [reldir|store|]

dbDir :: Path Rel Dir
dbDir = [reldir|coord|]

-- XXX: Reduce noise by piping stdout/stderr to a logfile.
spawnExecutor :: Path Abs Dir -> IO ProcessHandle
spawnExecutor wd = spawnProcess "ffexecutord"
  ["sqlite", fromAbsDir $ wd </> storeDir, fromAbsDir $ wd </> dbDir]

spawnExecutors :: Path Abs Dir -> Int -> IO [ProcessHandle]
spawnExecutors wd n = replicateM n (spawnExecutor wd)

killExecutors :: [ProcessHandle] -> IO ()
killExecutors = mapM_ terminateProcess

withExecutors :: Path Abs Dir -> Int -> IO a -> IO a
withExecutors wd n = bracket (spawnExecutors wd n) killExecutors . const

runTestFlow :: Path Abs Dir -> SimpleFlow a b -> a -> IO (Either SomeException b)
runTestFlow wd flow' input =
  CS.withStore (wd </> storeDir) $ \store ->
    runSimpleFlow SQLite (wd </> dbDir) store flow' input

echo :: SimpleFlow String CS.Item
echo = external $ \msg -> ExternalTask
  { _etCommand = "echo"
  , _etWriteToStdOut = True
  , _etParams = ["-n", fromString msg]
  }

flow :: SimpleFlow () String
flow = proc () -> do
  (a, (b, (c, d)))
    <- echo *** echo *** echo *** echo
    -< ("a", ("b", ("c", "d")))
  (e, (f, (g, h)))
    <- echo *** echo *** echo *** echo
    -< ("e", ("f", ("g", "h")))
  arr concat <<< mapA readString_ -< [a, b, c, d, e, f, g, h]

tests :: TestTree
tests = testGroup "SQLite Coordinator"
  [ testCase "echo flow" $
      withSystemTempDir "funflow_sqlite_" $ \wd ->
      withExecutors wd 4 $ do
        r <- runTestFlow wd flow ()
        case r of
          Left err -> assertFailure $ displayException err
          Right x  -> x @?= "abcdefgh"
  ]
