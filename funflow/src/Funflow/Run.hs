{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | This module defines how to run your flows
module Funflow.Run
  ( runFlow,
    runFlowWithConfig,
    RunFlowConfig (..),
  )
where

import Control.Arrow (Arrow, arr)
import Control.Exception.Safe (throw, throwString)
import Control.Kernmantle.Caching (localStoreWithId)
import Control.Kernmantle.Rope
  ( HasKleisliIO,
    LooseRopeWith,
    liftKleisliIO,
    perform,
    runReader,
    untwine,
    weave',
    (&),
    type (~>),
  )
import Control.Monad.Except (runExceptT)
import Data.CAS.ContentHashable (DirectoryContent (DirectoryContent))
import qualified Data.CAS.ContentStore as CS
import qualified Data.CAS.RemoteCache as RC
import Data.Either (isLeft, lefts, partitionEithers)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Map.Lazy as Map
import Data.Maybe (mapMaybe)
import Data.Profunctor.Trans (Reader, Writer, reading, runWriter, writing)
import Data.Set (fromList)
import Data.String (IsString (fromString))
import qualified Data.Text as T
import Docker.API.Client
  ( ContainerSpec (cmd),
    OS (OS),
    awaitContainer,
    defaultContainerSpec,
    hostVolumes,
    newDefaultDockerManager,
    pullImage,
    runContainer,
    saveContainerArchive,
    workingDir,
  )
import Funflow.Config (ConfigKeysBySource (..), Configurable (..), ExternalConfig (..), missing, readEnvs, readYamlFileConfig)
import Funflow.Flow (RequiredCore, RequiredStrands)
import Funflow.Run.Orphans ()
import Funflow.Tasks.Docker
  ( Arg (Arg, Placeholder),
    DockerTask (DockerTask),
    DockerTaskConfig (DockerTaskConfig),
    DockerTaskInput (DockerTaskInput),
    VolumeBinding (VolumeBinding),
    getIdFromArg,
    renderArg,
  )
import qualified Funflow.Tasks.Docker as DE
import Funflow.Tasks.Simple (SimpleTask (IOTask, PureTask))
import Funflow.Tasks.Store (StoreTask (GetDir, PutDir))
import GHC.Stack (HasCallStack)
import Network.HTTP.Client (Manager)
import Path (Abs, Dir, File, Path, absdir, parseRelDir, toFilePath, (</>))
import Path.IO (copyDirRecur)
import System.Directory (removeDirectory)
import System.Directory.Funflow (moveDirectoryContent)
import System.Info (os)
import System.PosixCompat.User (getEffectiveGroupID, getEffectiveUserID)

-- * Flow execution

-- | Flow execution configuration
data RunFlowConfig = RunFlowConfig
  { storePath :: Path Abs Dir,
    -- Optional config file for configuring task arguments
    configFile :: Maybe (Path Abs File)
  }

-- | Run a flow, parsing any required `Configurable` values from their respective sources.
-- Note that this method does NOT provide an implementation of parsing ConfigFromCLI
-- values at the moment.
runFlowWithConfig ::
  -- | The configuration of the flow
  RunFlowConfig ->
  -- | The flow to run
  LooseRopeWith RequiredStrands (RequiredCore IO) input output ->
  -- | The input to evaluate the flow with
  input ->
  IO output
runFlowWithConfig config flow input =
  let -- Expand config
      RunFlowConfig {storePath, configFile} = config
      defaultCachingId = Just 1
   in -- Run with store to enable caching (with default path to store)
      CS.withStore storePath $ \store -> do
        -- Start the manager required for docker
        manager <- newDefaultDockerManager (OS os)

        let -- Weave all strands
            weavedPipeline =
              flow
                -- Weave tasks
                & weave' #docker (interpretDockerTask manager store)
                & weave' #store (interpretStoreTask store)
                & weave' #simple interpretSimpleTask
                -- Strip of empty list of strands (after all weaves)
                & untwine

            -- At this point, the pipeline core is still wrapped in a couple of reader/writer layers.

            -- Extract all required external configs and docker images from DockerTasks
            (dockerImages, pipelineWithDockerConfigWriter) = runWriter weavedPipeline
            (dockerConfigs, pipelineWithDockerConfigReader) = runWriter pipelineWithDockerConfigWriter

            -- Finally, combine all config keys. You can plug in additional config keys from new task types here.
            requiredConfigs = mconcat [dockerConfigs]

        -- Run IO Actions to read config file, env vars, etc:
        fileConfig <- case configFile of
          Nothing -> return HashMap.empty
          Just path -> readYamlFileConfig $ toFilePath path
        envConfig <- readEnvs $ HashSet.toList $ envConfigKeys dockerConfigs
        -- TODO: Support for configurations via a CLI.
        let externalConfig = ExternalConfig {fileConfig = fileConfig, envConfig = envConfig, cliConfig = HashMap.empty}
            missingConfigs = missing externalConfig requiredConfigs

        -- At load-time, ensure that all expected configurations could be found.
        if not $ null missingConfigs
          then throwString $ "Missing the following required config keys: " ++ show missingConfigs
          else mempty :: IO ()

        -- Now, we can pass in configuration values to tasks which depend on them
        -- via a Reader layer.
        let -- Run reader layer for DockerTask configs and write out a list of any configuration error messages.
            (configErrors, weavePipeline') = runWriter $ runReader externalConfig pipelineWithDockerConfigReader

        -- If there were any additional configuration errors during interpretation, raise an exception.
        if not $ null configErrors
          then throwString $ "Configuration failed with errors: " ++ show configErrors
          else mempty :: IO ()

        let -- Run the reader layer for caching
            -- The `Just n` is a number that is used to compute caching hashes, changing it will recompute all
            core = runReader (localStoreWithId store defaultCachingId) weavePipeline'

        -- Pull docker images if there's any
        if not $ null dockerImages
          then do
            putStrLn "Found docker images, pulling..."
            let -- Remove duplicates by converting to a list
                dockerImagesSet = fromList dockerImages
                -- How we pull the docker image
                handleDockerImage image = do
                  putStrLn $ "Pulling docker image: " ++ T.unpack image
                  pullResult <- runExceptT $ pullImage manager image
                  case pullResult of
                    Left ex ->
                      throw ex
                    Right _ ->
                      -- No error, just continue
                      mempty :: IO ()
            mapM_ handleDockerImage dockerImagesSet
          else mempty

        -- At last, run the core
        perform input core

-- | Run a flow with the default configuration
runFlow ::
  -- | The flow to run
  LooseRopeWith RequiredStrands (RequiredCore IO) input output ->
  -- | The input to evaluate the flow with
  input ->
  IO output
runFlow = runFlowWithConfig (RunFlowConfig {storePath = [absdir|/tmp/funflow/store/|], configFile = Nothing})

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
          handleError hash = throwString $ "Could not put directory " <> show dirPath <> " in store item " <> show hash
          -- Copy recursively a directory from a DirectoryContent type
          copy :: Path Abs Dir -> DirectoryContent -> IO ()
          copy destinationPath (DirectoryContent sourcePath) = copyDirRecur sourcePath destinationPath
       in -- Use cas-store putInStore to generate the item in which to copy
          CS.putInStore store RC.NoCache handleError copy directoryContent
  GetDir ->
    -- Get path of item from store
    arr $ \item -> CS.itemPath store item

-- ** @DockerTask@ interpreter

-- | Interpret docker task
interpretDockerTask ::
  (Arrow core, HasKleisliIO m core, HasCallStack) =>
  Manager ->
  CS.ContentStore ->
  DockerTask i o ->
  (Writer [T.Text] ~> Writer ConfigKeysBySource ~> Reader ExternalConfig ~> Writer [String] ~> core) i o
interpretDockerTask manager store (DockerTask (DockerTaskConfig {DE.image, DE.command, DE.args})) =
  let requiredConfigs = mconcat $ mapMaybe getIdFromArg args
   in -- Add the image to the list of docker images stored in the Cayley Writer [T.Text]
      writing [image] $
        writing requiredConfigs $
          -- Read external configuration values and use them to populate the task's config
          reading $ \externalConfig ->
            let (configErrors, argsRenderedWithConfig) = partitionEithers $ map (renderArg externalConfig) args
             in -- Write any errors encountered during rendering of config so they can be thrown later
                writing configErrors $
                  -- Define the runtime behaviour (the core)
                  liftKleisliIO $ \(DockerTaskInput {DE.inputBindings, DE.argsVals}) ->
                    -- Check args placeholder fullfillment, right is value, left is unfullfilled label
                    let argsFilled =
                          [ ( case arg of
                                Arg configValue -> case configValue of
                                  Literal value -> Right value
                                  ConfigFromEnv k -> Left $ "interpretDockerTask encountered an unrendered externally configurable value at key: " ++ T.unpack k
                                  ConfigFromCLI k -> Left $ "interpretDockerTask encountered an unrendered externally configurable value at key: " ++ T.unpack k
                                  ConfigFromFile k -> Left $ "interpretDockerTask encountered an unrendered externally configurable value at key: " ++ T.unpack k
                                Placeholder label ->
                                  let maybeVal = Map.lookup label argsVals
                                   in case maybeVal of
                                        Nothing -> Left $ "Unfilled label" ++ label
                                        Just val -> Right val
                            )
                            | arg <- argsRenderedWithConfig
                          ]
                     in -- Error if one of the required configs or arg labels are not filled
                        if any isLeft argsFilled
                          then
                            let labelAndConfigErrors = lefts argsFilled
                             in throwString $ "Docker task failed with configuration errors: " ++ show labelAndConfigErrors
                          else do
                            let argsFilledChecked = [argVal | (Right argVal) <- argsFilled]
                            uid <- getEffectiveUserID
                            gid <- getEffectiveGroupID
                            let -- @defaultWorkingDirName@ has been chosen arbitrarly, it is both where Docker container will execute things, but also the exported folder to the content store
                                defaultWorkingDirName = "workdir"
                                defaultContainerWorkingDirPath = "/" ++ defaultWorkingDirName
                                container =
                                  (defaultContainerSpec image)
                                    { workingDir = T.pack defaultContainerWorkingDirPath,
                                      cmd = [command] <> argsFilledChecked,
                                      -- ":ro" suffix on docker binding means "read-only", the mounted volumes from the content store will not be modified
                                      hostVolumes = map fromString [(toFilePath $ CS.itemPath store item) <> ":" <> (toFilePath mount) <> ":ro" | VolumeBinding {DE.item, DE.mount} <- inputBindings]
                                    }
                            -- Run the docker container
                            runDockerResult <- runExceptT $ do
                              containerId <- runContainer manager container
                              awaitContainer manager containerId
                              return containerId
                            -- Process the result of the docker computation
                            case runDockerResult of
                              Left ex -> throw ex
                              Right containerId ->
                                let -- Define behaviors to pass to @CS.putInStore@
                                    handleError hash = throwString $ "Could not put in store item " ++ show hash
                                    copyDockerContainer itemPath _ = do
                                      copyResult <- runExceptT $ saveContainerArchive manager uid gid defaultContainerWorkingDirPath (toFilePath itemPath) containerId
                                      case copyResult of
                                        Left ex -> throw ex
                                        Right _ -> do
                                          -- Since docker will extract a TAR file of the container content, it creates a directory named after the requested directory's name
                                          -- In order to improve the user experience, funflow moves the content of said directory to the level of the CAS item directory
                                          itemWorkdir <- (itemPath </>) <$> (parseRelDir defaultWorkingDirName)
                                          moveDirectoryContent itemWorkdir itemPath
                                          -- After moving files and directories to item directory, remove the directory named after the working directory
                                          removeDirectory $ toFilePath itemWorkdir
                                 in CS.putInStore store RC.NoCache handleError copyDockerContainer (container, containerId, runDockerResult)
