{-
 - Entry point to Funflow
 -}
module Funflow
  ( -- Basics
    Flow,
    (>>>),
    -- Run flow
    FlowExecutionConfig (..),
    CommandExecutionHandler (..),
    defaultExecutionConfig,
    runFlow,
    -- "smart constructors" to make flows in an idiomatic way
    pureFlow,
    ioFlow,
    shellFlow,
    commandFlow,
    dockerFlow,
    nixFlow,
    -- Caching
    caching,
  )
where

import Control.Arrow ((>>>))
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
import Funflow.Run
  ( CommandExecutionHandler (..),
    FlowExecutionConfig (..),
    defaultExecutionConfig,
    runFlow,
  )
