{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- | This module defines how to run your flows
module Funflow.Run
  ( runFlow,
  )
where

import Control.Arrow (Arrow, arr)
import Control.Exception (bracket)
import Control.External
  ( Env (EnvExplicit),
    ExternalTask (..),
    OutputCapture (StdOutCapture),
    TaskDescription (..),
    outParam,
    uidParam,
  )
import Control.External.Executor (execute)
import Control.Kernmantle.Caching (localStoreWithId)
import Control.Kernmantle.Rope
  ( HasKleisliIO,
    liftKleisliIO,
    perform,
    runReader,
    untwine,
    (&),
    weave',
  )
import Control.Monad.IO.Class (liftIO)
import Data.CAS.ContentHashable (contentHash)
import qualified Data.CAS.ContentStore as CS
import Data.Either (isLeft)
import qualified Data.Map.Lazy as Map
import Data.String (fromString)
import qualified Data.Text as T
import Funflow.Effects.Docker
  ( Arg (Arg, Placeholder),
    DockerEffect (DockerEffect),
    DockerEffectConfig (DockerEffectConfig),
    DockerEffectInput (DockerEffectInput),
    VolumeBinding (VolumeBinding),
  )
import qualified Funflow.Effects.Docker as DE
import Funflow.Effects.Simple (SimpleEffect (..))
import Funflow.Flow (Flow)
import Katip
  ( ColorStrategy (ColorIfTerminal),
    Severity (InfoS),
    Verbosity (V2),
    closeScribes,
    defaultScribeSettings,
    initLogEnv,
    mkHandleScribe,
    permitItem,
    registerScribe,
    runKatipContextT,
  )
import Path (Abs, Dir, absdir, toFilePath)
import System.IO (stdout)

-- * Flow execution

-- | Run a flow
runFlow ::
  -- | The flow to run
  Flow input output ->
  -- | The input to evaluate the flow against
  input ->
  IO output
runFlow flow input =
  let -- TODO choose path
      defaultPath = [absdir|/tmp/funflow/store|]
      defaultCachingId = Just 1
   in -- Run with store to enable caching (with default path to store)
      CS.withStore defaultPath $ \store -> do
        flow
          -- Weave effects
          & weave' #docker (interpretDockerEffect store)
          & weave' #simple interpretSimpleEffect
          -- Strip of empty list of strands (after all weaves)
          & untwine
          -- Define the caching
          -- The `Just n` is a number that is used to compute caching hashes, changing it will recompute all
          & runReader (localStoreWithId store $ defaultCachingId)
          -- Finally, run
          & perform input

-- * Interpreters

-- ** @SimpleEffect@ interpreter

-- | Interpret @SimpleEffect@
interpretSimpleEffect :: (Arrow a, HasKleisliIO m a) => SimpleEffect i o -> a i o
interpretSimpleEffect simpleEffect = case simpleEffect of
  PureEffect f -> arr f
  IOEffect f -> liftKleisliIO f

-- ** @CommandEffect@ interpreters

-- | Run a task using external-executor, get a CS.Item
runTask :: CS.ContentStore -> ExternalTask -> IO CS.Item
runTask store task = do
  -- Hash computation, then bundle it with the task
  hash <- liftIO $ contentHash task
  -- Katip machinery
  handleScribe <- liftIO $ mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
  let makeLogEnv = registerScribe "stdout" handleScribe defaultScribeSettings =<< initLogEnv "funflow" "executor"
  _ <- bracket makeLogEnv closeScribes $ \logEnv -> do
    let initialContext = ()
    let initialNamespace = "funflow"
    runKatipContextT logEnv initialContext initialNamespace $
      execute store $
        TaskDescription
          { _tdOutput = hash,
            _tdTask = task
          }
  -- Finish
  CS.Complete completedItem <- CS.lookup store hash
  return completedItem

-- ** @DockerEffect@ interpreter

-- | Interpret docker effect
interpretDockerEffect :: (Arrow a, HasKleisliIO m a) => CS.ContentStore -> DockerEffect i o -> a i o
interpretDockerEffect store (DockerEffect (DockerEffectConfig {DE.image, DE.command, DE.args})) =
  liftKleisliIO $ \(DockerEffectInput {DE.inputBindings, DE.argsVals}) ->
    -- Check args placeholder fullfillment, right is value, left is unfullfilled label
    let argsFilled =
          [ ( case arg of
                Arg value -> Right value
                Placeholder label ->
                  let maybeVal = Map.lookup label argsVals
                   in case maybeVal of
                        Nothing -> Left label
                        Just val -> Right val
            )
            | arg <- args
          ]
     in -- Error if one of the required arg label is not filled
        if any isLeft argsFilled
          then
            let unfullfilledLabels = [label | (Left label) <- argsFilled]
             in -- TODO gracefully exit
                error $ "Missing arguments with labels: " ++ show unfullfilledLabels
          else do
            let argsFilledChecked = [argVal | (Right argVal) <- argsFilled]
             in runTask store $
                  ExternalTask
                    { _etCommand = "docker",
                      _etParams =
                        [ "run",
                          -- set the user in the container to the current user instead of root (prevent permission errors)
                          "--user=" <> uidParam,
                          -- set CWD in container
                          "--workdir=/workdir"
                        ]
                          -- bind output (which is also the CWD in the container)
                          ++ ["--volume=" <> outParam <> ":/workdir"]
                          -- volumes to bind
                          ++ [fromString $ "--volume=" <> (toFilePath $ CS.itemPath store item) <> ":/" <> (toFilePath mount) | VolumeBinding {DE.item, DE.mount} <- inputBindings]
                          ++ [ -- docker image
                               fromString . T.unpack $ image,
                               -- command
                               fromString . T.unpack $ command
                             ]
                          -- args
                          ++ map (fromString . T.unpack) argsFilledChecked,
                      _etEnv = EnvExplicit [],
                      _etWriteToStdOut = StdOutCapture
                    }