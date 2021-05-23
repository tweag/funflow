{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Run commands using Docker
module Funflow.Tasks.Docker
  ( DockerTaskConfig (..),
    DockerTask (..),
    DockerTaskInput (..),
    VolumeBinding (..),
    Arg (..),
    renderArg,
    getIdFromArg,
  )
where

import Data.CAS.ContentStore as CS
import Data.List (foldl')
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.String (IsString, fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Funflow.Config (ConfigKeysBySource, Configurable (Literal), ExternalConfig, configKeyBySource, render)
import Path (Abs, Dir, Path)

-- | Configure what task to run in Docker
data DockerTaskConfig = DockerTaskConfig
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
    Arg (Configurable Text)
  | -- | A placeholder for an argument to be passed as runtime input to the task (filled by @argsVals@)
    Placeholder String

instance IsString Arg where
  fromString s = Arg $ Literal $ T.pack s

-- | Extracts a ConfigKey from an Arg, if it exists.
getIdFromArg :: Arg -> Maybe ConfigKeysBySource
getIdFromArg arg = case arg of
  Arg configurable -> Just $ configKeyBySource configurable
  _ -> Nothing

-- | Renders an Arg with external configurations
renderArg :: ExternalConfig -> Arg -> Either String Arg
renderArg external arg = case arg of
  Arg configurable -> case render configurable external of
    Left err -> Left err
    Right renderedConfig -> Right $ Arg renderedConfig
  _ -> Right arg

-- | Input to a Docker task to finalize its configuration
data DockerTaskInput = DockerTaskInput
  { -- | Input items to mount on the container
    inputBindings :: [VolumeBinding],
    -- | A map representing how to fill the argument placeholders (placeholder label -> argument value)
    argsVals :: Map.Map String Text
  } deriving (Eq, Show)

instance Semigroup DockerTaskInput where
  DockerTaskInput{ inputBindings = vols1, argsVals = args1 } <> DockerTaskInput{ inputBindings = vols2, argsVals = args2} = 
    -- TODO: better to error for duplicate mounts? this treats merge like normal map.
    let agg (ms, vs) v = if Set.member (mount v) ms then (ms, vs) else (Set.insert (mount v) ms, v:vs)
        combVols       = reverse . snd $ foldl' agg (Set.empty, []) (vols1 ++ vols2)
    in DockerTaskInput{ inputBindings = combVols, argsVals = args1 <> args2 }

instance Monoid DockerTaskInput where
  mempty = DockerTaskInput{ inputBindings = [], argsVals = Map.empty }

-- | Represent how to bind a directory from cas-store (@CS.Item@) to a container internal file system
data VolumeBinding = VolumeBinding {item :: CS.Item, mount :: Path Abs Dir}
  deriving (Eq, Ord, Show)

-- Docker tasks to perform external tasks
data DockerTask i o where
  DockerTask :: DockerTaskConfig -> DockerTask DockerTaskInput CS.Item
