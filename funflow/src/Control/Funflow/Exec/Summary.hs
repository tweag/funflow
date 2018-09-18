{-# LANGUAGE TemplateHaskell #-}
-- | Execution summary.
--
--   This information is captured by funflow when running a flow, and may be
--   used by downstream consumers for analysis or display to users.

module Control.Funflow.Exec.Summary where

import Control.Lens
import Control.Lens.TH
import Control.Funflow.External (TaskDescription(..), ExternalTask(..))
import qualified Data.Text as T

type TimeStamp = Int

-- | The result of running a task.
data TaskResult
  = TaskExecuted
  | TaskResolvedFromCache
  | TaskFailed

-- | A summary of the results of running an internal task.
data InternalTaskSummary = InternalTaskSummary
  { _itsLabel ::  Maybe T.Text
  , _itsResult :: TaskResult
  , _itsStarted :: TimeStamp
  , _itsCompleted :: TimeStamp
  }

-- | A summary of the results of running an external task.
data ExternalTaskSummary = ExternalTaskSummary
  { _etsTask :: ExternalTask
  , _etsResult :: TaskResult
  , _etsSubmitted :: TimeStamp
  , _etsCompleted :: TimeStamp
  }

-- | Summary data about the execution of a flow.
data FlowSummary = FlowSummary
  { _fsSubmitted :: TimeStamp
  , _fsCompleted :: TimeStamp
  , _fsExternalTasks :: [ExternalTaskSummary]
  , _fsInternalTasks :: [InternalTaskSummary]
  }

makeLenses ''ExternalTaskSummary
makeLenses ''InternalTaskSummary
makeLenses ''FlowSummary
