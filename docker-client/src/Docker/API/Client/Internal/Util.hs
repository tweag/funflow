{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

-- | 'Util' contains various utility functions used throughout Docker.API.Client's internals.
module Docker.API.Client.Internal.Util where

import Data.Binary.Get (getWord32be, getWord8, runGet)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Conduit (ConduitT, await, yield)
import qualified Data.Conduit.Tar as Tar
import qualified Data.Text as T
import Docker.API.Client.Internal.Schemas (CreateContainer (..), HostConfig (..))
import Docker.API.Client.Internal.Types (ContainerLogType (..), ContainerSpec (..), DockerStreamType (..))
import System.Posix.Types (GroupID, UserID)

-- | (Internal only, do not export) Converts a client ContainerSpec into the format expected by the docker api
containerSpecToCreateContainer :: ContainerSpec -> CreateContainer
containerSpecToCreateContainer spec =
  CreateContainer
    { createContainerUser = textToMaybe $ user spec,
      createContainerWorkingDir = textToMaybe $ workingDir spec,
      createContainerEnv = listToMaybe $ envVars spec,
      createContainerImage = image spec,
      createContainerCmd = listToMaybe $ cmd spec,
      createContainerHostConfig = convertHostVolumes $ hostVolumes spec
    }
  where
    textToMaybe t = if not $ T.null t then Just t else Nothing
    listToMaybe l = if not $ null l then Just l else Nothing
    convertHostVolumes hvs =
      if null hvs
        then Nothing
        else Just HostConfig {hostConfigBinds = Just hvs}

-- | Conduit helper which checks the length of the input bytestring and reads data from upstream
-- until the length is at least nBytes. Note that this method may return a Bytestring result which
-- has length >= nBytes.
readUpstream :: Monad m => B.ByteString -> Int -> ConduitT B.ByteString o m (Maybe B.ByteString)
readUpstream acc nBytes =
  if B.length acc >= nBytes
    then do return $ Just acc
    else do
      result <- await
      case result of
        Nothing -> return Nothing
        Just val -> readUpstream (B.concat [acc, val]) nBytes

-- | Parses the first byte from a stream metadata bytestring returned by the Docker Engine API
-- and returns the corresponding stream type.
getStreamType :: B.ByteString -> DockerStreamType
getStreamType meta
  | indicator == 0 = DockerStreamStdIn
  | indicator == 1 = DockerStreamStdOut
  | indicator == 2 = DockerStreamStdErr
  | otherwise = error "Unrecognized stream type in docker engine response"
  where
    indicator = runGet getWord8 (LBS.fromStrict meta)

-- | Converts an 8 byte section length ByteString into an integer. This value
-- indicates the number of data bytes in the body of a Docker Engine stream record.
getSectionLength :: B.ByteString -> Int
getSectionLength = fromIntegral . runGet getWord32be . LBS.fromStrict

-- | Parses a docker metadata bytestring of length >= 8 into it's individual components
-- See https://docs.docker.com/engine/api/v1.40/#operation/ContainerAttach
parseDockerStream :: B.ByteString -> (DockerStreamType, Int, B.ByteString)
parseDockerStream bytes =
  let parts = splitToParts bytes
      streamType = getStreamType $ fst parts
      sectionLength = getSectionLength $ fst $ snd parts
      dataBytes = snd $ snd parts
   in (streamType, sectionLength, dataBytes)
  where
    splitToParts b = (B.singleton $ B.head b, B.splitAt 4 $ snd $ B.splitAt 4 b)

-- | Conduit for parsing a multiplexed stream from the Docker Engine API (e.g. the output of the attatch and logs endpoints).
-- This will force memory usage up to the returned frame size (for docker logs this is usually just one line of text).
-- See https://docs.docker.com/engine/api/v1.40/#operation/ContainerAttach for more details on this format.
parseMultiplexedDockerStream :: MonadIO m => ConduitT B.ByteString (DockerStreamType, B.ByteString) m ()
parseMultiplexedDockerStream = loop B.empty
  where
    loop acc = do
      -- This whole thing could be recursive
      -- This ensures that we get at least 8 bytes (could be more)
      input <- readUpstream acc 8
      case input of
        Nothing -> return ()
        Just meta -> do
          let (streamType, sectionLength, dataBytes) = parseDockerStream meta
          -- This ensures that we have at least as much data as our section (could be more)
          section <- readUpstream dataBytes sectionLength
          case section of
            Nothing -> return ()
            Just s -> do
              let (expectedBytes, additionalBytes) = B.splitAt sectionLength s
              yield (streamType, expectedBytes)
              loop additionalBytes

-- | Creates a DockerStreamType filter using the input ContainerLogType
createLogTypeFilter :: ContainerLogType -> ((DockerStreamType, a) -> Bool)
createLogTypeFilter clt = case clt of
  Stdout -> \(t, _) ->
    case t of
      DockerStreamStdOut -> True
      _ -> False
  StdErr -> \(t, _) ->
    case t of
      DockerStreamStdErr -> True
      _ -> False
  Both -> \(t, _) ->
    case t of
      _ -> True

-- | Replaces the user and group id for an entry in a tarball with the specified user and group
chownTarballContent :: UserID -> GroupID -> Tar.FileInfo -> Tar.FileInfo
chownTarballContent uid gid info =
  info
    { Tar.fileUserName = "",
      Tar.fileUserId = uid,
      Tar.fileGroupName = "",
      Tar.fileGroupId = gid
    }
