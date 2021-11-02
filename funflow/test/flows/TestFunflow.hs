{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Control.Exception.Safe (SomeException)
import qualified Data.CAS.ContentStore as CS
import qualified Data.Map as Map
import qualified Data.Text as Text
import Docker.API.Client (DockerClientError (ContainerCreationFailedError))
import Funflow
  ( Flow,
    RunFlowConfig (..),
    caching,
    dockerFlow,
    getDirFlow,
    ioFlow,
    pureFlow,
    putDirFlow,
    runFlowWithConfig,
    tryE,
  )
import Funflow.Config (Configurable (ConfigFromEnv, ConfigFromFile, Literal))
import Funflow.Tasks.Docker (Arg, DockerTaskConfig (DockerTaskConfig), DockerTaskInput (DockerTaskInput), VolumeBinding (VolumeBinding))
import qualified Funflow.Tasks.Docker as DE
import Path (Abs, Dir, File, Rel, absdir, parseAbsDir, reldir, relfile, (</>))
import System.Directory (getCurrentDirectory)
import System.Environment (setEnv)

main :: IO ()
main = do
  -- Run multiple flows to test funflow's capabilities
  putStr "\n------ TESTING ------\n"
  testFlow @Int @Int "a flow from a pure function" somePureFlow 0
  putStr "\n---------------------\n"
  testFlow @() @() "a flow with IO" someIoFlow ()
  putStr "\n---------------------\n"
  testFlow @() @() "a flow with caching" someCachedFlow ()
  putStr "\n---------------------\n"
  testFlow @() @() "a flow copying a directory to the store" someStoreFlow ()
  putStr "\n---------------------\n"
  testFlow @() @CS.Item "a flow running a task in docker" someDockerFlow ()
  putStr "\n---------------------\n"
  testFlow @() @CS.Item "a flow running a task in docker using a configurable from a file" someDockerFlowWithFileConfig ()
  putStr "\n---------------------\n"
  testFlow @() @CS.Item "a flow running a task in docker using a configurable from an env var" someDockerFlowWithEnvConfig ()
  putStr "\n---------------------\n"
  testFlow @() @CS.Item "a flow running a task in docker, using the output of one as input of another" someDockerFlowWithInputs ()
  putStr "\n---------------------\n"
  testFlow @() @() "a flow running a task in docker which fails, but error is caught by the pipeline" someDockerFlowThatFails ()
  putStr "\n---------------------\n"
  testFlow @() @() "a flow with input keys not in config causes no problem" dockerFlowWithExtraInputKeys ()
  putStr "\n------  DONE   ------\n"

testFlow :: forall i o. (Show i, Show o) => String -> Flow i o -> i -> IO ()
testFlow label flow input = do
  -- Set an environment variable for config tests
  setEnv "FUNFLOW_TEST" "print('funflow is great!')"
  -- Get current working directory as Path Abs Dir
  cwd <- parseAbsDir =<< getCurrentDirectory
  let storeDirPath = cwd </> [reldir|./.tmp/store|]
      configFilePath = cwd </> [relfile|./test/flows/assets/flow.yaml|]
      runFlow :: Flow i o -> i -> IO o
      runFlow = runFlowWithConfig (RunFlowConfig {configFile = Just configFilePath, storePath = storeDirPath})
  putStrLn $ "Testing " ++ label
  putStrLn $ "Store opened at " <> show storeDirPath
  result <- runFlow flow input
  putStrLn $ "Got " ++ (show result) ++ " from input " ++ (show input)

someCachedFlow :: Flow () ()
someCachedFlow = proc () -> do
  () <- caching ("someComputation" :: String) $ ioFlow $ (\() -> putStrLn "This message should appear at most once") -< ()
  () <- caching ("someComputation" :: String) $ ioFlow $ (\() -> putStrLn "This message should appear at most once") -< ()
  ioFlow $ (\() -> putStrLn "If nothing printed, then it works") -< ()

somePureFlow :: Flow Int Int
somePureFlow = pureFlow $ (+ 1)

someIoFlow :: Flow () ()
someIoFlow = ioFlow $ const $ putStrLn "Some IO operation"

someStoreFlow :: Flow () ()
someStoreFlow = proc () -> do
  -- Prepare the test
  -- Note: the relative path is specific to running the test with Nix with `$(nix-build nix -A funflow.components.tests)/bin/test-funflow`
  --   which is the case in the CI
  testDir <- ioFlow (\() -> return . flip (</>) [reldir|./test/flows/assets/storeFlowTest/|] =<< parseAbsDir =<< getCurrentDirectory) -< ()
  -- The actual test
  item <- putDirFlow -< testDir
  path <- getDirFlow -< item
  ioFlow $ (\(item, itemDirPath) -> putStrLn $ "Copied directory to item " <> show item <> " with path " <> show itemDirPath) -< (item, path)

someDockerFlow :: Flow () CS.Item
someDockerFlow = proc () -> do
  dockerFlow (DockerTaskConfig {DE.image = "python:latest", DE.command = "python", DE.args = ["-c", "print('someDockerFlow worked')"]}) -< DockerTaskInput {DE.inputBindings = [], DE.argsVals = mempty}

someDockerFlowWithFileConfig :: Flow () CS.Item
someDockerFlowWithFileConfig = proc () -> do
  dockerFlow (DockerTaskConfig {DE.image = "python:latest", DE.command = "python", DE.args = ["-c", DE.Arg $ ConfigFromFile "python.command"]}) -< DockerTaskInput {DE.inputBindings = [], DE.argsVals = mempty}

someDockerFlowWithEnvConfig :: Flow () CS.Item
someDockerFlowWithEnvConfig = proc () -> do
  dockerFlow (DockerTaskConfig {DE.image = "python:latest", DE.command = "python", DE.args = ["-c", DE.Arg $ ConfigFromEnv "FUNFLOW_TEST"]}) -< DockerTaskInput {DE.inputBindings = [], DE.argsVals = mempty}

someDockerFlowWithInputs :: Flow () CS.Item
someDockerFlowWithInputs = proc () -> do
  item <- dockerFlow (DockerTaskConfig {DE.image = "python:latest", DE.command = "python", DE.args = ["-c", "with open('test.py', 'w') as f: f.write('print(\\'Hello world\\')')"]}) -< DockerTaskInput {DE.inputBindings = [], DE.argsVals = mempty}
  dockerFlow (DockerTaskConfig {DE.image = "python:latest", DE.command = "python", DE.args = ["/script/test.py"]}) -< DockerTaskInput {DE.inputBindings = [VolumeBinding {DE.item = item, DE.mount = [absdir|/script/|]}], DE.argsVals = mempty}

someDockerFlowThatFails :: Flow () ()
someDockerFlowThatFails =
  let someFailingDockerFlow = dockerFlow (DockerTaskConfig {DE.image = "python:latest", DE.command = "badCommand", DE.args = []})
      -- A helper function for the test
      isContainerCreationFailedError (ContainerCreationFailedError _) = True
      isContainerCreationFailedError _ = False
   in proc () -> do
        result <- tryE @DockerClientError someFailingDockerFlow -< DockerTaskInput {DE.inputBindings = [], DE.argsVals = mempty}
        case result of
          Left ex ->
            if isContainerCreationFailedError ex
              then ioFlow (\ex -> putStrLn $ "Exception caught: " ++ show ex) -< ex
              else ioFlow $ const $ error "Wrong exception caught!" -< ()
          Right _ -> ioFlow $ const $ error "Exception not caught!" -< ()

dockerFlowWithExtraInputKeys :: Flow () ()
dockerFlowWithExtraInputKeys =
  let usedArgs = [("a", "first"), ("b", "second")]
      pars = map fst usedArgs
      taskConf = DockerTaskConfig {DE.image = "bash:latest", DE.command = "echo", DE.args = map DE.Placeholder pars}
      taskIn = DockerTaskInput {DE.inputBindings = [], DE.argsVals = Map.fromList (("extra_1", "oh!") : (usedArgs ++ [("extra_2", "no!")]))}
   in proc () -> do
        result <- tryE @SomeException (dockerFlow taskConf) -< taskIn
        case result of
          Left ex -> ioFlow (\err -> error ("Exception! " ++ show err)) -< ex
          Right _ -> ioFlow $ const $ putStrLn "All good!" -< ()
