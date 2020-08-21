{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import qualified Data.CAS.ContentStore as CS
import Funflow
  ( Flow,
    caching,
    dockerFlow,
    ioFlow,
    pureFlow,
    runFlow,
  )
import Funflow.Effects.Docker (DockerEffectConfig (DockerEffectConfig), DockerEffectInput (DockerEffectInput), VolumeBinding (VolumeBinding))
import qualified Funflow.Effects.Docker as DE
import Path (Abs, Dir, absdir)

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
  testFlow @() @CS.Item "a flow running a task in docker" someDockerFlow ()
  putStr "\n---------------------\n"
  testFlow @() @CS.Item "a flow running a task in docker, using the output of one as input of another" someDockerFlowWithInputs ()
  putStr "\n------  DONE   ------\n"

testFlow :: forall i o. (Show i, Show o) => String -> Flow i o -> i -> IO ()
testFlow label flow input = do
  putStrLn $ "Testing " ++ label
  result <- runFlow @i @o flow input
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

someDockerFlow :: Flow () CS.Item
someDockerFlow = proc () -> do
  dockerFlow (DockerEffectConfig {DE.image = "python", DE.command = "python", DE.args = ["-c", "print('someDockerFlow worked')"]}) -< DockerEffectInput {DE.inputBindings = [], DE.argsVals = mempty}

someDockerFlowWithInputs :: Flow () CS.Item
someDockerFlowWithInputs = proc () -> do
  item <- dockerFlow (DockerEffectConfig {DE.image = "python", DE.command = "python", DE.args = ["-c", "with open('test.py', 'w') as f: f.write('print(\\'Hello world\\')')"]}) -< DockerEffectInput {DE.inputBindings = [], DE.argsVals = mempty}
  dockerFlow (DockerEffectConfig {DE.image = "python", DE.command = "python", DE.args = ["/script/test.py"]}) -< DockerEffectInput {DE.inputBindings = [VolumeBinding {DE.item = item, DE.mount = [absdir|/script/|]}], DE.argsVals = mempty}
