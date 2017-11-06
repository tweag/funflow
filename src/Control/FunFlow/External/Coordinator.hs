{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
module Control.FunFlow.External.Coordinator where

import           Control.FunFlow.ContentHashable (ContentHash)
import           Control.Lens
import           Control.Monad.IO.Class          (MonadIO, liftIO)

import qualified Data.ByteString                 as B
import           Data.Store                      (Store)
import           Data.Store.TH                   (makeStore)


import           Network.HostName
import           System.Clock                    (TimeSpec)

instance Store TimeSpec

-- | Information about an executor capable of running tasks. Currently this
--   is just a newtype wrapper around hostname.
newtype Executor = Executor HostName
  deriving Store

data TaskDescription = TaskDescription {
    _tdOutput     :: ContentHash
  , _tdSerialised :: B.ByteString
  }

data TaskStatus =
    -- | Task is in the queue and has not begun executing
    Pending
  | Running ExecutionInfo
  | Completed ExecutionInfo
    -- | Task has failed with failure count
  | Failed ExecutionInfo Int

data TaskInfo = TaskInfo {
    _tiStatus :: TaskStatus
  }

data ExecutionInfo = ExecutionInfo {
    _eiExecutor :: Executor
  , _eiElapsed  :: TimeSpec
  }

class Coordinator c where
  type Config c
  type Hook c = h | h -> c

  -- | Perform any necessary initialisation to connect to the coordinator.
  initialise :: MonadIO m => Config c -> m (Hook c)

  -- | Submit a task to the task queue.
  submitTask :: MonadIO m => Hook c -> TaskDescription -> m ()

  -- | View the size of the current task queue
  queueSize :: MonadIO m => Hook c -> m Int

  -- | Fetch information on the current task
  taskInfo :: MonadIO m => Hook c -> ContentHash -> m (Maybe TaskInfo)

  -- | Pop a task off of the queue for execution. The popped task should be
  --   added to the execution queue
  popTask :: MonadIO m => Hook c -> Executor
          -> m (Maybe TaskDescription)

  -- | Await task completion. If the task is not running or an invalid
  --   content hash is submitted, this will return Nothing immediately.
  --   If the task has failed, this will also return 'Nothing'.
  awaitTask :: MonadIO m => Hook c -> ContentHash -> m (Maybe ExecutionInfo)

  -- | Update execution status for a running task.
  --   This should error for a task which is not running.
  updateTaskStatus :: MonadIO m => Hook c -> ContentHash -> TaskStatus -> m ()

-- TH Splices

makeLenses ''TaskDescription
makeLenses ''TaskInfo
makeLenses ''ExecutionInfo
makeStore ''TaskStatus
makeStore ''ExecutionInfo
makeStore ''TaskInfo

-- Derived functionality

startTask :: (Coordinator c, MonadIO m)
          => Hook c
          -> m (Maybe TaskDescription)
startTask h = liftIO $ do
  executorInfo <- Executor <$> getHostName
  popTask h executorInfo
