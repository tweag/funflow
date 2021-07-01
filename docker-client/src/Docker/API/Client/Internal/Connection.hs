{-# LANGUAGE OverloadedStrings #-}

-- | HTTP Connection managers and utilities for connecting to the Docker Engine API
module Docker.API.Client.Internal.Connection where

import Docker.API.Client.Internal.Types (OS (..))
import Network.HTTP.Client
import Network.HTTP.Client.Internal (makeConnection)
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as SBS
import System.IO (FilePath)

-- | Default docker socket path on unix systems
defaultDockerUnixSocket :: FilePath
defaultDockerUnixSocket = "/var/run/docker.sock"

-- | Creates a new HTTP connection manager for the default docker daemon address
-- on your system.
newDefaultDockerManager :: OS -> IO Manager
-- TODO: default uri and manager for windows (which uses tcp instead of a socket)
newDefaultDockerManager (OS "mingw32") = undefined
newDefaultDockerManager _ = newUnixDomainSocketManager defaultDockerUnixSocket

-- | Creates a new http connection manager from a file path to a unix socket
newUnixDomainSocketManager :: FilePath -> IO Manager
newUnixDomainSocketManager path = do
  -- Stolen from: https://kseo.github.io/posts/2017-01-23-custom-connection-manager-for-http-client.html
  let mSettings = defaultManagerSettings {managerRawConnection = return $ openUnixSocket path}
  newManager mSettings
  where
    openUnixSocket filePath _ _ _ = do
      s <- S.socket S.AF_UNIX S.Stream S.defaultProtocol
      S.connect s (S.SockAddrUnix filePath)
      makeConnection
        (SBS.recv s 8096)
        (SBS.sendAll s)
        (S.close s)
