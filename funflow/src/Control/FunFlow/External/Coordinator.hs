{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
module Control.FunFlow.External.Coordinator where

import           Control.FunFlow.ContentHashable (ContentHash)
import           Control.FunFlow.External
import           Control.Lens
import           Control.Monad.IO.Class          (MonadIO, liftIO)

import           Data.Store                      (Store)
import           Data.Store.TH                   (makeStore)


import           Network.HostName
import           System.Clock                    (TimeSpec)

instance Store TimeSpec

-- | Information about an executor capable of running tasks. Currently this
--   is just a newtype wrapper around hostname.
newtype Executor = Executor HostName
  deriving (Show, Store)

data TaskStatus =
    -- | Task is in the queue and has not begun executing
    Pending
  | Running ExecutionInfo
  | Completed ExecutionInfo
    -- | Task has failed with failure count
  | Failed ExecutionInfo Int
  deriving Show

data TaskInfo =
    KnownTask TaskStatus
  | UnknownTask

data ExecutionInfo = ExecutionInfo {
    _eiExecutor :: Executor
  , _eiElapsed  :: TimeSpec
  } deriving Show

class Coordinator c where
  type Config c
  type Hook c = h | h -> c

  -- | Perform any necessary initialisation to connect to the coordinator.
  initialise :: MonadIO m => Config c -> m (Hook c)

  -- | Submit a task to the task queue.
  --   It is allowed to overwrite a known task.
  submitTask :: MonadIO m => Hook c -> TaskDescription -> m ()

  -- | View the size of the current task queue
  queueSize :: MonadIO m => Hook c -> m Int

  -- | Fetch information on the current task
  taskInfo :: MonadIO m => Hook c -> ContentHash -> m TaskInfo

  -- | Pop a task off of the queue for execution. The popped task should be
  --   added to the execution queue
  popTask :: MonadIO m => Hook c -> Executor
          -> m (Maybe TaskDescription)

  -- | Await task completion.
  --
  --   If the task is complete, this will return 'KnownTask Completed'.
  --   If the task is failed, this will return 'KnownTask Failed'.
  --   If the task is not known to the system, this will return 'UnknownTask'.
  --   Otherwise (if the task is pending or running), this will block until
  --   the task either completes or fails.
  awaitTask :: MonadIO m => Hook c -> ContentHash -> m TaskInfo

  -- | Update execution status for a running task.
  --   This should error for a task which is not running.
  updateTaskStatus :: MonadIO m => Hook c -> ContentHash -> TaskStatus -> m ()

  -- | Remove all pending tasks from the queue.
  dropTasks :: MonadIO m => Hook c -> m ()

-- TH Splices

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

-- | Check if a task is currently 'in progress' - e.g.
--   pending or running.
isInProgress :: (Coordinator c, MonadIO m)
          => Hook c
          -> ContentHash
          -> m Bool
isInProgress h ch = do
  ti <- taskInfo h ch
  return $ case ti of
    KnownTask Pending     -> True
    KnownTask (Running _) -> True
    _                     -> False
