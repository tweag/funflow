{-
 - Entry point to Funflow
 -}
module Funflow
  ( -- Basics
    Flow,
    FlowExecutionConfig (..),
    CommandExecutionHandler (..),
    runFlow,
    -- Caching
    caching,
    -- Helpers to make flows in an idiomatic way
    pureFlow,
    ioFlow,
    shellFlow,
    commandFlow,
    dockerFlow,
    nixFlow,
  )
where

import Control.Kernmantle.Caching (caching)
import Funflow.Flow (Flow)
import Funflow.Flows
  ( commandFlow,
    dockerFlow,
    ioFlow,
    nixFlow,
    pureFlow,
    shellFlow,
  )
import Funflow.Run (CommandExecutionHandler (..), FlowExecutionConfig (..), runFlow)
