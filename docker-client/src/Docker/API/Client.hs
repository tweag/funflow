module Docker.API.Client
  ( dockerAPIVersion,
    OS (..),
    defaultDockerUnixSocket,
    newDefaultDockerManager,
    newUnixDomainSocketManager,
    DockerClientError (..),
    ClientErrorMonad,
    ContainerSpec (..),
    defaultContainerSpec,
    runContainer,
    saveContainerArchive,
    removeContainer,
    saveContainerLogs,
    ContainerLogType (..),
    pullImage,
  )
where

import Docker.API.Client.Internal.Connection
  ( defaultDockerUnixSocket,
    newDefaultDockerManager,
    newUnixDomainSocketManager,
  )
import Docker.API.Client.Internal.Requests
  ( dockerAPIVersion,
    pullImage,
    removeContainer,
    runContainer,
    saveContainerArchive,
    saveContainerLogs,
  )
import Docker.API.Client.Internal.Types
  ( ClientErrorMonad,
    ContainerLogType (..),
    ContainerSpec (..),
    DockerClientError (..),
    OS (..),
    defaultContainerSpec,
  )
