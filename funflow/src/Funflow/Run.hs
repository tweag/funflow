{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
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
    runFlowWithConfig,
    RunFlowConfig (..),
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
import Data.CAS.ContentHashable (DirectoryContent (DirectoryContent), contentHash)
import qualified Data.CAS.ContentStore as CS
import qualified Data.CAS.RemoteCache as RC
import Data.Either (isLeft)
import qualified Data.Map.Lazy as Map
import Data.String (fromString)
import qualified Data.Text as T
import Funflow.Tasks.Docker
  ( Arg (Arg, Placeholder),
    DockerTask (DockerTask),
    DockerTaskConfig (DockerTaskConfig),
    DockerTaskInput (DockerTaskInput),
    VolumeBinding (VolumeBinding),
  )
import qualified Funflow.Tasks.Docker as DE
import Funflow.Tasks.Simple (SimpleTask (IOTask, PureTask))
import Funflow.Tasks.Store (StoreTask (GetDir, PutDir))
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
import Path (Abs, Dir, Path, absdir, toFilePath)
import System.IO (stdout)
import Path.IO (copyDirRecur)

-- * Flow execution

-- | Flow execution configuration
data RunFlowConfig = RunFlowConfig {storePath :: Path Abs Dir}

-- | Run a flow
runFlowWithConfig ::
  -- | The configuration of the flow
  RunFlowConfig ->
  -- | The flow to run
  Flow input output ->
  -- | The input to evaluate the flow with
  input ->
  IO output
runFlowWithConfig config flow input =
  let -- Expand config
      (RunFlowConfig {storePath}) = config
      defaultCachingId = Just 1
   in -- Run with store to enable caching (with default path to store)
      CS.withStore storePath $ \store -> do
        flow
          -- Weave tasks
          & weave' #docker (interpretDockerTask store)
          & weave' #store (interpretStoreTask store)
          & weave' #simple interpretSimpleTask
          -- Strip of empty list of strands (after all weaves)
          & untwine
          -- Define the caching
          -- The `Just n` is a number that is used to compute caching hashes, changing it will recompute all
          & runReader (localStoreWithId store $ defaultCachingId)
          -- Finally, run
          & perform input

-- | Run a flow with the default configuration
runFlow ::
  -- | The flow to run
  Flow input output ->
  -- | The input to evaluate the flow with
  input ->
  IO output
runFlow = runFlowWithConfig (RunFlowConfig {storePath = [absdir|/tmp/funflow/store/|]})

-- * Interpreters

-- ** @SimpleTask@ interpreter

-- | Interpret @SimpleTask@
interpretSimpleTask :: (Arrow a, HasKleisliIO m a) => SimpleTask i o -> a i o
interpretSimpleTask simpleTask = case simpleTask of
  PureTask f -> arr f
  IOTask f -> liftKleisliIO f

-- ** @StoreTask@ interpreters

-- | Interpret @StoreTask@
interpretStoreTask :: (Arrow a, HasKleisliIO m a, RC.Cacher m RC.NoCache) => CS.ContentStore -> StoreTask i o -> a i o
interpretStoreTask store storeTask = case storeTask of
  PutDir ->
    liftKleisliIO $ \dirPath ->
      let -- Use the DirectoryContent type
          -- this will give a hash through `ContentHashable` that takes into account the content of the directory
          directoryContent = DirectoryContent dirPath
          -- Handle errors
          handleError hash = error $ "Could not put directory " <> show dirPath <> " in store item " <> show hash
          -- Copy recursively a directory from a DirectoryContent type
          copy :: Path Abs Dir -> DirectoryContent -> IO ()
          copy destinationPath (DirectoryContent sourcePath) = copyDirRecur sourcePath destinationPath
       in -- Use cas-store putInStore to generate the item in which to copy
          CS.putInStore store RC.NoCache handleError copy directoryContent
  GetDir ->
    -- Get path of item from store
    arr $ \item -> CS.itemPath store item

-- ** @DockerTask@ interpreter

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

-- | Interpret docker task
interpretDockerTask :: (Arrow a, HasKleisliIO m a) => CS.ContentStore -> DockerTask i o -> a i o
interpretDockerTask store (DockerTask (DockerTaskConfig {DE.image, DE.command, DE.args})) =
  liftKleisliIO $ \(DockerTaskInput {DE.inputBindings, DE.argsVals}) ->
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