{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Functions for sending HTTP requests to the Docker Engine API
module Docker.API.Client.Internal.Requests where

import Conduit (filterC, mapC)
import Control.Monad.Except
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Conduit (runConduit, (.|))
import Data.Conduit.Combinators (sinkFile, sinkNull, stdout)
import qualified Data.Conduit.Tar as Tar
import Data.Conduit.Zlib (ungzip)
import qualified Data.Text as T
import Docker.API.Client.Internal.Schemas (ContainerId, CreateContainer (..), CreateContainerResponse (..), WaitContainerResponse (..))
import Docker.API.Client.Internal.Types (ClientErrorMonad, ContainerLogType (..), ContainerSpec (image), DockerClientError (..))
import Docker.API.Client.Internal.Util (chownTarballContent, containerSpecToCreateContainer, createLogTypeFilter, parseMultiplexedDockerStream)
import Network.HTTP.Client
import qualified Network.HTTP.Conduit as HTTPC
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Status (Status, status200, status201, status204, statusCode, statusMessage)
import System.Posix.Types (GroupID, UserID)

-- | Docker Engine API version. This value will prefix all docker api url paths.
dockerAPIVersion :: String
dockerAPIVersion = "v1.40"

-- | Helper function which formats the response of a failed HTTP request
formatRequestError :: Status -> LBS.ByteString -> String
formatRequestError status body =
  "Request failed with status "
    ++ show (statusCode status)
    ++ ": "
    ++ show (statusMessage status)
    ++ " "
    ++ show body

-- | Similar to the `docker run` command. Runs a container in the background using the input HTTP connection manager, returning immediately.
-- To wait for the container to exit use `awaitContainer`.
-- Note that this currently always tries to pull the container's image.
runContainer ::
  -- | The connection manager for the docker daemon. You can `Docker.API.Client.newDefaultDockerManager` to get a default
  -- connection manager based on your operating system.
  Manager ->
  ContainerSpec ->
  ClientErrorMonad ContainerId
runContainer manager spec =
  let payload = containerSpecToCreateContainer spec
   in pullImage manager (image spec) >> submitCreateContainer manager payload >>= parseCreateContainerResult >>= startContainer manager

-- | Waits on a started container (e.g. via `runContainer`) until it exits, validating its exit code and returning an `DockerClientError` if
-- the container exited with an error. This will work for both actively running containers and those which have already exited.
awaitContainer ::
  -- | The connection manager for the docker daemon. You can `Docker.API.Client.newDefaultDockerManager` to get a default
  -- connection manager based on your operating system.
  Manager ->
  -- | The container id to await
  ContainerId ->
  ClientErrorMonad ()
awaitContainer manager cid = submitWaitContainer manager cid >>= parseWaitContainerResult >>= checkExitStatusCode

-- | Analagous to the `docker cp` command. Recursively copies contents at the specified path in the container to
-- the provided output path on the host, setting file permissions to the specified user and group id.  Note that the
-- container must have been started for this to succeed (i.e. it must have a running or finished state).  This method
-- uses conduit to optimize memory usage.
saveContainerArchive ::
  -- | The connection manager for the docker daemon
  Manager ->
  -- | The user id to use for output files and directories
  UserID ->
  -- | The group id to use for output files and directories
  GroupID ->
  -- | The path in the container to copy
  FilePath ->
  -- | The output path at which to write outputs
  FilePath ->
  ContainerId ->
  ClientErrorMonad ()
saveContainerArchive manager uid gid itemPath outPath cid = do
  let request =
        setQueryString [("path", Just (B.pack itemPath))] $
          defaultRequest
            { method = "GET",
              path = B.pack $ "/" ++ dockerAPIVersion ++ "/containers/" ++ cid ++ "/archive"
            }
  result <- liftIO $
    runResourceT $ do
      response <- HTTPC.http request manager
      let body = HTTPC.responseBody response
      let status = HTTPC.responseStatus response
      -- Note: we're using a different approach than in the other http requests since the response
      -- is wrapped in a ResourceT when using http-conduit, and we would have to implement
      -- the conduit pipeline using ExceptT to make it compatible with that approach.
      if status == status200
        then do
          -- This operation may throw an unexpected exception. If we want to catch stuff like
          -- tar decoding errors we can switch to Tar.restoreFileIntoLenient
          runConduit $ body .| ungzip .| Tar.untar (Tar.restoreFileInto outPath . chownTarballContent uid gid) .| sinkNull
          return Nothing
        else return $ Just $ GetContainerArchiveError $ formatRequestError status ""
  case result of
    Just e -> throwError e
    Nothing -> return ()

-- | Pulls an image from a remote registry (similar to a `docker pull` command). This currently
-- only supports public registries (e.g. DockerHub).
pullImage ::
  -- | The connection manager for the docker daemon
  Manager ->
  -- | The image of interest. May include an optional tag or digest field. Note that
  -- in line with the Docker Engine API, this will pull **ALL** images in a repo if no tag
  -- or digest is specified.
  T.Text ->
  ClientErrorMonad ()
pullImage manager image = do
  let request =
        setQueryString [("fromImage", Just (B.pack $ T.unpack image))] $
          defaultRequest
            { method = "POST",
              path = B.pack $ "/" ++ dockerAPIVersion ++ "/images/create"
            }
  response <- liftIO $ httpLbs request manager
  let result
        | status == status200 = return ()
        | otherwise = throwError $ ImagePullError $ formatRequestError status body
        where
          body = responseBody response
          status = responseStatus response
  result

-- | Streams the logs from a docker container into the specified output file path. Logs can include
-- stdout, stderr, or both. Note that if you include both streams, the sorting of the timestamps in the output
-- file may not be perfectly sorted since the stream returned by the docker api is only sorted within each
-- stream type (i.e. stdout and stderr are sorted separately).
saveContainerLogs ::
  -- | The connection manager for the docker daemon
  Manager ->
  -- | Which logs to fetch from the container
  ContainerLogType ->
  -- | Output file at which to write logs
  FilePath ->
  ContainerId ->
  ClientErrorMonad ()
saveContainerLogs manager logType outPath cid = do
  let request =
        setQueryString
          [ ("follow", Just "false"),
            ("stdout", Just stdout),
            ("stderr", Just stderr),
            ("timestamps", Just "true")
          ]
          $ defaultRequest
            { method = "GET",
              path = B.pack $ "/" ++ dockerAPIVersion ++ "/containers/" ++ cid ++ "/logs"
            }
        where
          stderr = case logType of
            Stdout -> "false"
            _ -> "true"
          stdout = case logType of
            StdErr -> "false"
            _ -> "true"
  result <- liftIO $
    runResourceT $ do
      response <- HTTPC.http request manager
      let body = HTTPC.responseBody response
      let status = HTTPC.responseStatus response
      -- Note: we're using a different approach than in the other http requests since the response
      -- is wrapped in a ResourceT when using http-conduit, and we would have to implement
      -- the conduit pipeline using ExceptT to make it compatible with that approach.
      if status == status200
        then do
          let isLogType = createLogTypeFilter logType
          runConduit $ body .| parseMultiplexedDockerStream .| filterC isLogType .| mapC snd .| sinkFile outPath
          return Nothing
        else return $ Just $ GetContainerLogsError $ formatRequestError status ""
  case result of
    Just e -> throwError e
    Nothing -> return ()

-- | Streams the logs from a docker container, printing them to stdout. Logs can include
-- stdout, stderr, or both.
printContainerLogs ::
  -- | The connection manager for the docker daemon
  Manager ->
  -- | Which logs to fetch from the container
  ContainerLogType ->
  ContainerId ->
  ClientErrorMonad ()
printContainerLogs manager logType cid = do
  let request =
        setQueryString
          [ ("follow", Just "true"),
            ("stdout", Just stdout),
            ("stderr", Just stderr),
            ("timestamps", Just "true")
          ]
          $ defaultRequest
            { method = "GET",
              path = B.pack $ "/" ++ dockerAPIVersion ++ "/containers/" ++ cid ++ "/logs"
            }
        where
          stderr = case logType of
            Stdout -> "false"
            _ -> "true"
          stdout = case logType of
            StdErr -> "false"
            _ -> "true"
  result <- liftIO $
    runResourceT $ do
      response <- HTTPC.http request manager
      let body = HTTPC.responseBody response
      let status = HTTPC.responseStatus response
      -- Note: we're using a different approach than in the other http requests since the response
      -- is wrapped in a ResourceT when using http-conduit, and we would have to implement
      -- the conduit pipeline using ExceptT to make it compatible with that approach.
      if status == status200
        then do
          let isLogType = createLogTypeFilter logType
          runConduit $ body .| parseMultiplexedDockerStream .| filterC isLogType .| mapC snd .| stdout
          return Nothing
        else return $ Just $ GetContainerLogsError $ formatRequestError status ""
  case result of
    Just e -> throwError e
    Nothing -> return ()

-- | Attempts to create a docker container, returning the new container's id
submitCreateContainer :: Manager -> CreateContainer -> ClientErrorMonad LBS.ByteString
submitCreateContainer manager object = do
  let reqBody = RequestBodyLBS $ encode object
  let request =
        defaultRequest
          { method = "POST",
            path = B.pack $ "/" ++ dockerAPIVersion ++ "/containers/create",
            requestBody = reqBody,
            requestHeaders = [(hContentType, "application/json")]
          }
  response <- liftIO $ httpLbs request manager
  let result
        -- In the ExceptT world, return is used where you would use Right
        | status == status201 = return body
        -- And throwError is used where you would use Left
        | otherwise = throwError $ ContainerCreationFailedError $ formatRequestError status body
        where
          body = responseBody response
          status = responseStatus response
  result

startContainer :: Manager -> ContainerId -> ClientErrorMonad ContainerId
startContainer manager cid = do
  let request =
        defaultRequest
          { method = "POST",
            path = B.pack $ "/" ++ dockerAPIVersion ++ "/containers/" ++ cid ++ "/start"
          }
  response <- liftIO $ httpLbs request manager
  let result
        -- In the ExceptT world, return is used where you would use Right
        | status == status204 = return cid
        -- And throwError is used where you would use Left
        | otherwise = throwError $ ContainerCreationFailedError $ formatRequestError status body
        where
          body = responseBody response
          status = responseStatus response
  result

submitWaitContainer :: Manager -> ContainerId -> ClientErrorMonad LBS.ByteString
submitWaitContainer manager cid = do
  let request =
        defaultRequest
          { method = "POST",
            path = B.pack $ "/" ++ dockerAPIVersion ++ "/containers/" ++ cid ++ "/wait"
          }
  response <- liftIO $ httpLbs request manager
  let result
        -- In the ExceptT world, return is used where you would use Right
        | status == status200 = return $ responseBody response
        -- And throwError is used where you would use Left
        | otherwise = throwError $ ContainerCreationFailedError $ formatRequestError status body
        where
          body = responseBody response
          status = responseStatus response
  result

-- | Remove a container, equivalent to the `docker container rm` command
removeContainer ::
  -- | The connection manager for the docker daemon
  Manager ->
  -- | Enable force removal?
  Bool ->
  -- | Also remove container's volumes?
  Bool ->
  ContainerId ->
  ClientErrorMonad ContainerId
removeContainer manager isForceful alsoRemoveVolumes cid = do
  let request =
        setQueryString [("force", Just force), ("v", Just v)] $
          defaultRequest
            { method = "DELETE",
              path = B.pack $ "/" ++ dockerAPIVersion ++ "/containers/" ++ cid
            }
        where
          force = if isForceful then "true" else "false"
          v = if alsoRemoveVolumes then "true" else "false"
  response <- liftIO $ httpLbs request manager
  let result
        | status == status204 = return cid
        | otherwise = throwError $ ContainerCreationFailedError $ formatRequestError status body
        where
          body = responseBody response
          status = responseStatus response
  result

-- | Parses the response body of a create container request
parseCreateContainerResult :: LBS.ByteString -> ClientErrorMonad ContainerId
parseCreateContainerResult body = case eitherDecode body of
  Left msg -> throwError $ UnrecognizedJSONResponseError msg
  Right object -> return $ createContainerResponseId object

-- | Parses the response body of an await container request
parseWaitContainerResult :: LBS.ByteString -> ClientErrorMonad WaitContainerResponse
parseWaitContainerResult body = case eitherDecode body of
  Left msg -> throwError $ UnrecognizedJSONResponseError msg
  Right object -> return object

-- | Checks the status code value of a WaitContainerResponse
checkExitStatusCode :: WaitContainerResponse -> ClientErrorMonad ()
checkExitStatusCode response = do
  if code == 0
    then return ()
    else case waitContainerResponseError response of
      -- Docker will only sometimes return the associated errors
      Just err -> throwError $ NonZeroExitCode $ show err
      Nothing -> throwError $ NonZeroExitCode $ "Container status code was " ++ show code ++ " with no error message"
  where
    code = waitContainerResponseStatusCode response
