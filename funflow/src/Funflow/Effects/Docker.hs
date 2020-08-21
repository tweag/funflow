{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

-- | Run commands using Docker
module Funflow.Effects.Docker
  ( DockerEffectConfig (..),
    DockerEffect (..),
    DockerEffectInput (..),
    VolumeBinding (..),
    Arg (..),
  )
where

import Data.CAS.ContentStore as CS
import Data.Map (Map)
import Data.String (IsString, fromString)
import Data.Text (Text)
import Path (Abs, Dir, Path)
import qualified Data.Text as T

-- | Configure what task to run in Docker
data DockerEffectConfig = DockerEffectConfig
  { -- | The name of the docker image
    image :: Text,
    -- | The command to run
    command :: Text,
    -- | The arguments to pass to the command run inside of the container
    args :: [Arg]
  }

-- | Represent an argument to pass to the command run inside of a Docker container
data Arg
  = -- | Raw text argument
    Arg Text
  | -- | A placeholder for an argument to be passed as runtime input to the effect (filled by @argsVals@)
    Placeholder String

-- | Allow to write fixed textual arguments directly as strings
instance IsString Arg where
  fromString = Arg . T.pack

-- | Input to a Docker effect to finalize its configuration
data DockerEffectInput = DockerEffectInput
  { -- | Input items to mount on the container
    inputBindings :: [VolumeBinding],
    -- | A map representing how to fill the argument placeholders (placeholder label -> argument value)
    argsVals :: Map String Text
  }

-- | Represent how to bind a directory from cas-store (@CS.Item@) to a container internal file system
data VolumeBinding = VolumeBinding {item :: CS.Item, mount :: Path Abs Dir}

-- Docker effects to perform external tasks
data DockerEffect i o where
  DockerEffect :: DockerEffectConfig -> DockerEffect DockerEffectInput CS.Item
