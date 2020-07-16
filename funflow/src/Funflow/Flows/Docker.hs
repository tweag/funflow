{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

{-
 - "Docker" flows allow to run tasks in Docker
 -}
module Funflow.Flows.Docker where

import Data.Text (Text)

-- Configure what task to run in Docker
data DockerFlowConfig = DockerFlowConfig
  { image :: Text,
    command :: Text,
    args :: [Text]
  }

-- Docker flows to perform external tasks
data DockerFlow i o where
  DockerFlow :: DockerFlowConfig -> DockerFlow () ()
