{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Funflow
  ( CommandExecutionHandler (SystemExecutor),
    Flow,
    FlowExecutionConfig (FlowExecutionConfig),
    SimpleEffect (..),
    CommandEffect(..),
    DockerEffect(..),
    NixEffect(..),
    caching,
    commandExecution,
    runFlow,
    toFlow,
  )
import Funflow.Effects.Command (CommandEffectConfig (CommandEffectConfig))
import qualified Funflow.Effects.Command as CF
import Funflow.Effects.Docker (DockerEffectConfig (DockerEffectConfig))
import qualified Funflow.Effects.Docker as DF
import qualified Funflow.Effects.Nix as NF

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
  testFlow @() @() "a flow running a shell string command" someShellFlow ()
  putStr "\n---------------------\n"
  testFlow @() @() "a flow running a command" someCommandFlow ()
  putStr "\n---------------------\n"
  testFlow @() @() "a flow running a task in docker" someDockerFlow ()
  putStr "\n---------------------\n"
  testFlow @() @() "a flow running a task in a nix shell" someNixFlow ()
  putStr "\n------  DONE   ------\n"

testFlowExecutionConfig :: FlowExecutionConfig
testFlowExecutionConfig = FlowExecutionConfig {commandExecution = SystemExecutor}

testFlow :: forall i o. (Show i, Show o) => String -> Flow i o -> i -> IO ()
testFlow label flow input = do
  putStrLn $ "Testing " ++ label
  result <- runFlow @i @o testFlowExecutionConfig flow input
  putStrLn $ "Got " ++ (show result) ++ " from input " ++ (show input)

someCachedFlow :: Flow () ()
someCachedFlow = proc () -> do
  () <- caching ("someComputation" :: String) $ toFlow . IOEffect $ (\() -> putStrLn "This message should appear at most once") -< ()
  () <- caching ("someComputation" :: String) $ toFlow . IOEffect $ (\() -> putStrLn "This message should appear at most once") -< ()
  toFlow . IOEffect $ (\() -> putStrLn "If nothing printed, then it works") -< ()

somePureFlow :: Flow Int Int
somePureFlow = toFlow . PureEffect $ (+ 1)

someIoFlow :: Flow () ()
someIoFlow = toFlow . IOEffect $ const $ putStrLn "Some IO operation"

someShellFlow :: Flow () ()
someShellFlow = toFlow . ShellCommandEffect $ "echo someShellFlow worked"

someCommandFlow :: Flow () ()
someCommandFlow = toFlow . CommandEffect $ (CommandEffectConfig {CF.command = "echo", CF.args = ["someCommandFlow worked"], CF.env = []})

someDockerFlow :: Flow () ()
someDockerFlow = toFlow . DockerEffect $ (DockerEffectConfig {DF.image = "python", DF.command = "python", DF.args = ["-c", "print('someDockerFlow worked')"]})

someNixFlow :: Flow () ()
someNixFlow = toFlow . NixEffect $ (NF.NixEffectConfig {NF.nixEnv = NF.PackageList ["python"], NF.command = "python -c \"print('someNixFlow worked')\"", NF.args = [], NF.env = [], NF.nixpkgsSource = NF.NIX_PATH})
