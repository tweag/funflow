{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

{-
 - "Command" effects allow to run commands
 -}
module Funflow.Effects.Command
  ( CommandEffectConfig (..),
    CommandEffect (..),
  )
where

import Data.Text (Text)

-- Configure what command to run
data CommandEffectConfig = CommandEffectConfig
  { command :: Text,
    args :: [Text],
    env :: [(Text, Text)]
  }

-- Command effects to run a command
data CommandEffect i o where
  CommandEffect :: CommandEffectConfig -> CommandEffect () ()
  ShellCommandEffect :: Text -> CommandEffect () ()
