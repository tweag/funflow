{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

{-
 - "Command" flows allow to run commands
 -}
module Funflow.Flows.Command where

import Data.Text (Text)

-- Configure what command to run
data CommandFlowConfig = CommandFlowConfig
  { command :: Text,
    args :: [Text],
    env :: [(Text, Text)]
  }

-- Command flows to run a command
data CommandFlow i o where
  CommandFlow :: CommandFlowConfig -> CommandFlow () ()
  ShellCommandFlow :: Text -> CommandFlow () ()
