{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Except (runExceptT)
import Data.Either (isLeft, isRight)
import Data.List
import Data.Ord
import qualified Data.Text as T
import Docker.API.Client (ContainerLogType (..), ContainerSpec (..), OS (..), awaitContainer, defaultContainerSpec, newDefaultDockerManager, pullImage, removeContainer, runContainer, saveContainerArchive, saveContainerLogs)
import GHC.IO.Handle (Handle)
import Network.HTTP.Client (Manager)
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, listDirectory)
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import System.Info (os)
import System.PosixCompat.User (getEffectiveGroupID, getEffectiveUserID)
import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [withResource manager cleanupManager dockerIntegrationTests, unitTests]

manager :: IO Manager
manager = newDefaultDockerManager (OS os)

-- This is required for using tasy's withResource, but doesn't
-- actually need to do anything since Managers handle closing
-- connections internally.
cleanupManager :: Manager -> IO ()
cleanupManager m = return ()

testImage :: T.Text
testImage = "alpine:20200626"

containerWithSuccessfulCommand :: ContainerSpec
containerWithSuccessfulCommand =
  (defaultContainerSpec testImage)
    { cmd = ["sh", "-c", "exit 0"]
    }

containerWithFailingCommand :: ContainerSpec
containerWithFailingCommand =
  (defaultContainerSpec testImage)
    { cmd = ["sh", "-c", "exit 1"]
    }

containerWithValidHostBindVolume :: FilePath -> Handle -> ContainerSpec
containerWithValidHostBindVolume p _ =
  (defaultContainerSpec testImage)
    { cmd = ["sh", "-c", "ls /test-host-bind"],
      hostVolumes = [T.concat [T.pack p, ":/test-host-bind"]]
    }

containerWithInvalidHostBindVolume :: ContainerSpec
containerWithInvalidHostBindVolume =
  (defaultContainerSpec testImage)
    { cmd = ["sh", "-c", "ls /test-host-bind"],
      hostVolumes = ["/does/not/exist/on/host:/test-host-bind"]
    }

containerWithKnownFile :: ContainerSpec
containerWithKnownFile =
  (defaultContainerSpec testImage)
    { cmd = ["sh", "-c", "echo foo > /test-file.txt"]
    }

containerWithKnownDirectory :: ContainerSpec
containerWithKnownDirectory =
  (defaultContainerSpec testImage)
    { cmd = ["sh", "-c", "mkdir -p /test-dir/subdir && echo foo > /test-dir/test-file.txt"]
    }

containerWithLogOutputs :: ContainerSpec
containerWithLogOutputs =
  (defaultContainerSpec testImage)
    { -- This cmd will generate 10 lines of stdout and 10 lines of stderr outputs
      cmd = ["sh", "-c", "for i in `seq 1 10`; do echo foo && echo thisisanerror >&2; done"]
    }

countLinesInFile :: FilePath -> IO Int
countLinesInFile path = readFile path >>= (return . length . lines)

formatLineCountMessage :: Int -> Int -> String
formatLineCountMessage actual expected = "Number of lines in output log file was (" ++ show actual ++ "), when we expected " ++ "(" ++ show expected ++ ")"

dockerIntegrationTests :: IO Manager -> TestTree
dockerIntegrationTests managerIO =
  testGroup
    "Docker integration tests"
    [ testCase "Pull a test image" $ do
        manager <- managerIO
        result <- runExceptT $ pullImage manager testImage
        assertBool ("Failed with error " ++ show result) $ isRight result,
      testCase "Run container valid command" $ do
        manager <- managerIO
        result <- runExceptT $ runContainer manager containerWithSuccessfulCommand >>= awaitContainer manager
        assertBool ("Failed with error " ++ show result) $ isRight result,
      testCase "Run container invalid command" $ do
        manager <- managerIO
        result <- runExceptT $ runContainer manager containerWithFailingCommand >>= awaitContainer manager
        assertBool "Running container succeeded when it should have actually failed" $ isLeft result,
      testCase "Run container with valid host bind volume" $ do
        manager <- managerIO
        result <- withSystemTempFile "docker-client-bind" (\p h -> runExceptT $ runContainer manager (containerWithValidHostBindVolume p h) >>= awaitContainer manager)
        assertBool ("Failed with error " ++ show result) $ isRight result,
      testCase "Run and remove a container" $ do
        manager <- managerIO
        result <- runExceptT $ do
          containerId <- runContainer manager containerWithSuccessfulCommand
          awaitContainer manager containerId
          removeContainer manager False False containerId
        assertBool ("Failed with error " ++ show result) $ isRight result,
      testCase "Run and remove a container forcefully" $ do
        manager <- managerIO
        result <- runExceptT $ do
          containerId <- runContainer manager containerWithSuccessfulCommand
          awaitContainer manager containerId
          removeContainer manager True False containerId
        assertBool ("Failed with error " ++ show result) $ isRight result,
      testCase "Run and remove a container plus its volumes" $ do
        manager <- managerIO
        result <- runExceptT $ do
          containerId <- runContainer manager containerWithSuccessfulCommand
          awaitContainer manager containerId
          removeContainer manager False True containerId
        assertBool ("Failed with error " ++ show result) $ isRight result,
      testCase "Run and remove a container plus its volumes forcefully" $ do
        manager <- managerIO
        result <- runExceptT $ do
          containerId <- runContainer manager containerWithSuccessfulCommand
          awaitContainer manager containerId
          removeContainer manager True True containerId
        assertBool ("Failed with error " ++ show result) $ isRight result,
      testCase "Run a container and extract a file from it" $ do
        manager <- managerIO
        -- The temporary package only lets you write things with the permissions of the current user
        uid <- getEffectiveUserID
        gid <- getEffectiveGroupID
        (success, msg) <-
          withSystemTempDirectory
            "docker-client-cp-file"
            ( \p -> do
                result <- runExceptT $ do
                  containerId <- runContainer manager containerWithKnownFile
                  awaitContainer manager containerId
                  saveContainerArchive manager uid gid "/test-file.txt" p containerId
                case result of
                  Left e -> return (False, show e)
                  Right _ -> do
                    outputExists <- doesFileExist (p ++ "/test-file.txt")
                    return (outputExists, "Could not find expected file after extraction")
            )
        assertBool msg success,
      testCase "Run a container and extract a directory from it" $ do
        manager <- managerIO
        -- The temporary package only lets you write things with the permissions of the current user
        uid <- getEffectiveUserID
        gid <- getEffectiveGroupID
        (success, msg) <-
          withSystemTempDirectory
            "docker-client-cp-dir"
            ( \p -> do
                result <- runExceptT $ do
                  containerId <- runContainer manager containerWithKnownDirectory
                  awaitContainer manager containerId
                  saveContainerArchive manager uid gid "/test-dir" p containerId
                case result of
                  Left e -> return (False, show e)
                  Right _ -> do
                    fileOutputExists <- doesFileExist (p ++ "/test-dir/test-file.txt")
                    dirOutputExists <- doesDirectoryExist (p ++ "/test-dir/subdir")
                    return (fileOutputExists && dirOutputExists, "Could not find expected file and subdirectory after extraction")
            )
        assertBool msg success,
      testCase "Run a container and extract its STDOUT logs" $ do
        manager <- managerIO
        (success, msg) <-
          withSystemTempDirectory
            "docker-client-logs"
            ( \p -> do
                let expectedLines = 10
                let logFile = p ++ "/stdout.txt"
                result <- runExceptT $ do
                  containerId <- runContainer manager containerWithLogOutputs
                  awaitContainer manager containerId
                  saveContainerLogs manager Stdout logFile containerId
                case result of
                  Left e -> return (False, show e)
                  Right _ -> do
                    nLines <- countLinesInFile logFile
                    return (nLines == expectedLines, formatLineCountMessage nLines expectedLines)
            )
        assertBool msg success,
      testCase "Run a container and extract its STDERR logs" $ do
        manager <- managerIO
        (success, msg) <-
          withSystemTempDirectory
            "docker-client-logs"
            ( \p -> do
                let expectedLines = 10
                let logFile = p ++ "/stderr.txt"
                result <- runExceptT $ do
                  containerId <- runContainer manager containerWithLogOutputs
                  awaitContainer manager containerId
                  saveContainerLogs manager StdErr logFile containerId
                case result of
                  Left e -> return (False, show e)
                  Right _ -> do
                    nLines <- countLinesInFile logFile
                    return (nLines == expectedLines, formatLineCountMessage nLines expectedLines)
            )
        assertBool msg success,
      testCase "Run a container and extract both its STDOUT + STDERR logs" $ do
        manager <- managerIO
        (success, msg) <-
          withSystemTempDirectory
            "docker-client-logs"
            ( \p -> do
                let expectedLines = 20
                let logFile = p ++ "/both-logs.txt"
                result <- runExceptT $ do
                  containerId <- runContainer manager containerWithLogOutputs
                  awaitContainer manager containerId
                  saveContainerLogs manager Both logFile containerId
                case result of
                  Left e -> return (False, show e)
                  Right _ -> do
                    nLines <- countLinesInFile logFile
                    return (nLines == expectedLines, formatLineCountMessage nLines expectedLines)
            )
        assertBool msg success
    ]

unitTests :: TestTree
unitTests = testGroup "Unit" []
