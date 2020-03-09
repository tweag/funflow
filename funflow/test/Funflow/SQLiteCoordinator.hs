{-# LANGUAGE Arrows            #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Funflow.SQLiteCoordinator where

import           Control.Arrow
import           Control.Arrow.Free
import           Control.Concurrent                          (threadDelay)
import           Control.Concurrent.Async                    (wait, withAsync)
import           Control.Concurrent.MVar
import           Control.Exception.Safe
import           Control.Funflow
import           Control.Funflow.External.Coordinator.SQLite
import           Control.Monad
import qualified Data.CAS.ContentStore                as CS
import           Data.Semigroup                              ((<>))
import           Data.String                                 (fromString)
import           Path
import           Path.IO
import qualified System.Posix.Signals                        as Signals
import           System.Posix.Types                          (ProcessID)
import           System.Process
import qualified System.Process.Internals                    as Process
import           System.Timeout                              (timeout)
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

withExecutors :: Path Abs Dir -> Int -> ([ProcessHandle] -> IO a) -> IO a
withExecutors wd n = bracket (spawnExecutors wd n) killExecutors

withExecutors_ :: Path Abs Dir -> Int -> IO a -> IO a
withExecutors_ wd n = withExecutors wd n . const

getProcessID :: ProcessHandle -> IO (Maybe ProcessID)
getProcessID ph = Process.withProcessHandle ph $ \case
  Process.OpenHandle h -> return (Just h)
  Process.OpenExtHandle h _ _ -> return (Just h)
  Process.ClosedHandle {} -> return Nothing

signalProcess :: Signals.Signal -> ProcessHandle -> IO ()
signalProcess sig = getProcessID >=> \case
  Nothing -> return ()
  Just pid -> Signals.signalProcess sig pid

runTestFlow :: Path Abs Dir -> SimpleFlow a b -> a -> IO (Either SomeException b)
runTestFlow wd flow' input =
  CS.withStore (wd </> storeDir) $ \store ->
    runSimpleFlow SQLite (wd </> dbDir) store flow' input

echo :: SimpleFlow String CS.Item
echo = external $ \msg -> ExternalTask
  { _etCommand = "echo"
  , _etWriteToStdOut = StdOutCapture
  , _etParams = ["-n", fromString msg]
  , _etEnv = EnvExplicit []
  }

sleepEcho :: SimpleFlow (Double, String) CS.Item
sleepEcho = external $ \(time, msg) -> ExternalTask
  { _etCommand = "sh"
  , _etWriteToStdOut = StdOutCapture
  , _etParams =
      [ "-c"
      , "sleep " <> fromString (show time) <> ";"
        <> "echo -n " <> fromString msg
      ]
  , _etEnv = EnvExplicit []
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
      withExecutors_ wd 4 $ do
        r <- runTestFlow wd flow ()
        case r of
          Left err -> assertFailure $ displayException err
          Right x  -> x @?= "abcdefgh"
  , testCase "interrupt worker" $
      withSystemTempDir "funflow_sqlite_" $ \wd -> do
        r <- timeout 10000000 $
          -- Spawn one initial executor.
          withExecutors wd 1 $ \[executorHandle] -> do
            mvar <- newMVar False
            let trigger :: SimpleFlow () ()
                trigger = stepIO (\_ -> modifyMVar_ mvar $ \_ -> pure True)
                sleepFlow :: SimpleFlow () String
                sleepFlow = proc () -> do
                  r <- sleepEcho -< (1, "test")
                  trigger -< const () r
                  readString_ -< r
            -- Run the flow in parallel.
            withAsync (runTestFlow wd sleepFlow ()) $ \flowAsync -> do
              threadDelay 500000
              -- Interrupt the executor while the external task is running.
              signalProcess Signals.sigINT executorHandle
              -- Send a second interrupt shortly after. Users can be impatient.
              -- GHC's default interrupt handler is one-time, after the first
              -- interrupt was called a second will terminate the process
              -- immediately, leaving no time for clean-up.
              threadDelay 50000
              signalProcess Signals.sigINT executorHandle
              threadDelay 2000000
              -- Check that the executor did not complete the task.
              progress <- readMVar mvar
              when progress $
                assertFailure "Executor should not have completed the task"
              -- Spawn a new executor to finish the flow.
              withExecutors wd 1 $ \_ ->
                wait flowAsync
        case r of
          Nothing -> assertFailure "Timed out"
          Just (Left err) -> assertFailure $ displayException err
          Just (Right x)  -> x @?= "test"
  ]
