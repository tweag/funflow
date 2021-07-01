{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Aeson schemas for communicating with the Docker Engine API
module Docker.API.Client.Internal.Schemas where

import Data.Aeson
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | Constructs default Aeson options for serializing and deserializing objects in this module. This method
-- drops the first nPrefix characters from record accessors in your Haskell record before serializing to
-- JSON and uses the shortened field names to match record accessors when deserializing.
dockerParseOptions :: Int -> Options
dockerParseOptions nPrefix = defaultOptions {fieldLabelModifier = drop nPrefix}

-- | A Docker container's unique ID (in longer format)
type ContainerId = String

-----------------------------------------------------------------------------
--------------------- BEGIN LONG LIST OF SCHEMAS ----------------------------
-----------------------------------------------------------------------------

data CreateContainer = CreateContainer
  { createContainerUser :: Maybe T.Text,
    createContainerEnv :: Maybe [T.Text],
    createContainerCmd :: Maybe [T.Text],
    createContainerImage :: T.Text,
    createContainerWorkingDir :: Maybe T.Text,
    createContainerHostConfig :: Maybe HostConfig
  }
  deriving (Generic, Show)

-- TODO: Write a method for generating binds which is a bit more safe
data HostConfig = HostConfig
  { hostConfigBinds :: Maybe [T.Text]
  }
  deriving (Generic, Show)

data CreateContainerResponse = CreateContainerResponse
  { createContainerResponseId :: ContainerId,
    createContainerResponseWarnings :: Maybe [T.Text]
  }
  deriving (Generic, Show)

data WaitContainerResponse = WaitContainerResponse
  { waitContainerResponseStatusCode :: Int,
    waitContainerResponseError :: Maybe WaitContainerError
  }
  deriving (Generic, Show)

data WaitContainerError = WaitContainerError
  { waitContainerErrorMessage :: T.Text
  }
  deriving (Generic, Show)

-----------------------------------------------------------------------------
----------- BEGIN LONG LIST OF SERIALIZER/DESERIALIZER INSTANCES-------------
-----------------------------------------------------------------------------

instance FromJSON HostConfig where parseJSON = genericParseJSON $ dockerParseOptions 10

instance ToJSON HostConfig where
  toJSON = genericToJSON $ dockerParseOptions 10
  toEncoding = genericToEncoding $ dockerParseOptions 10

instance FromJSON CreateContainerResponse where parseJSON = genericParseJSON $ dockerParseOptions 23

instance ToJSON CreateContainerResponse where
  toJSON = genericToJSON $ dockerParseOptions 23
  toEncoding = genericToEncoding $ dockerParseOptions 23

instance FromJSON CreateContainer where parseJSON = genericParseJSON $ dockerParseOptions 15

instance ToJSON CreateContainer where
  toJSON = genericToJSON $ dockerParseOptions 15
  toEncoding = genericToEncoding $ dockerParseOptions 15

instance FromJSON WaitContainerResponse where parseJSON = genericParseJSON $ dockerParseOptions 21

instance ToJSON WaitContainerResponse where
  toJSON = genericToJSON $ dockerParseOptions 21
  toEncoding = genericToEncoding $ dockerParseOptions 21

instance FromJSON WaitContainerError where parseJSON = genericParseJSON $ dockerParseOptions 18

instance ToJSON WaitContainerError where
  toJSON = genericToJSON $ dockerParseOptions 18
  toEncoding = genericToEncoding $ dockerParseOptions 18
