{-# LANGUAGE OverloadedStrings #-}

module Docker.API.Client.Internal.Types where

import Control.Monad.Except
import qualified Data.Text as T

-- | Alias for the system type returned by System.Info.os
newtype OS = OS String

-- | This type describes common errors the docker client might encounter
data DockerClientError
  = -- | The request to create a new container failed
    ContainerCreationFailedError String
  | -- | The docker engine API responded with something we didn't expect
    UnrecognizedJSONResponseError String
  | -- | The docker engine API responded with an error status code when getting a container archive
    GetContainerArchiveError String
  | -- | The container exited with a nonzero exit code
    NonZeroExitCode String
  | -- | The docker engine API responded with an error when we attempted to get a container's logs
    GetContainerLogsError String
  | -- | The docker engine API responded with an error when we attempted to pull an image
    ImagePullError String
  deriving (Show)

-- | Wrapper for composing operations which return an Either DockerClientError a
type ClientErrorMonad a = ExceptT DockerClientError IO a

-- | Describes a docker container to be created with runContainer. Use defaultContainerSpec to
-- get a default value.
data ContainerSpec = ContainerSpec
  { -- | The image name with an optional tag or digest field (e.g. "python:3.6")
    image :: T.Text,
    -- | The container's command. If empty will default to the image default.
    cmd :: [T.Text],
    -- | Optional user ID to use in the container. If empty will default to the image default.
    user :: T.Text,
    -- | Optional working directory in the container. If empty will default to the image default.
    workingDir :: T.Text,
    -- | Optional list of environment variables of format "FOO=BAR".
    envVars :: [T.Text],
    -- | Optional list of host volumes to mount to the container as bind mounts. Must follow
    -- the format specied here: https://docs.docker.com/storage/bind-mounts/
    hostVolumes :: [T.Text]
  }

-- | Constructs a simple default ContainerSpec for a docker image which uses the image's default
-- values for all other aguments.
defaultContainerSpec ::
  -- | The container's Docker Image. May optionally include a tag or digest field (e.g. "python:3.6").
  T.Text ->
  ContainerSpec
defaultContainerSpec img =
  ContainerSpec
    { image = img,
      cmd = [],
      user = "",
      workingDir = "",
      envVars = [],
      hostVolumes = []
    }

data ContainerLogType = Stdout | StdErr | Both

-- | Possible types of streams returned by the Docker Engine API attach and logs endpoints. Used only internally
-- for parsing the stream metadata returned by docker.
data DockerStreamType = DockerStreamStdIn | DockerStreamStdOut | DockerStreamStdErr
