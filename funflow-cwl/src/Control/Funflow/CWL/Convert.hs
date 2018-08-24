{-|
Module      : Convert
Description : This module re-exports the key functions that convert
  internal data types into funflow workflows.

This module re-exports the key functions that convert
internal data types into funflow workflows.

-}

module Control.Funflow.CWL.Convert
  ( convertTool
  , convertWorkflow
  , convertCWLFile
  , convertJob
  , SomeJob (..)
  ) where

import           Control.Funflow.CWL.Convert.Job
import           Control.Funflow.CWL.Convert.Tool
import           Control.Funflow.CWL.Convert.Workflow



