{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

{-
 - "Docker" effects allow to run tasks in Docker
 -}
module Funflow.Effects.Docker
  ( DockerEffectConfig (..),
    DockerEffect (..),
  )
where

import Data.Text (Text)

-- Configure what task to run in Docker
data DockerEffectConfig = DockerEffectConfig
  { image :: Text,
    command :: Text,
    args :: [Text]
  }

-- Docker effects to perform external tasks
data DockerEffect i o where
  DockerEffect :: DockerEffectConfig -> DockerEffect () ()
