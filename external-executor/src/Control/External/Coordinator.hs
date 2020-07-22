{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | A Funflow coordinator is used to distribute tasks amongst multiple
--   executors. It provides a functionality to submit tasks, to fetch them for
--   execution, and to check on their status.
--
--   There are multiple possible instantiations of the 'Coordinator' class.
module Control.External.Coordinator where

import Control.Exception.Safe
import Control.External
import Control.Lens
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.CAS.ContentHashable (ContentHash)
import Data.Store (Store)
import Data.Store.TH (makeStore)
import Katip
import Network.HostName
import Path
import System.Clock (TimeSpec)

#if !MIN_VERSION_store(0,5,0)
instance Store TimeSpec
#endif

-- | Information about an executor capable of running tasks. Currently this
--   is just a newtype wrapper around hostname.
newtype Executor = Executor HostName
  deriving (Show, Store)

data TaskStatus
  = -- | Task is in the queue and has not begun executing
    Pending
  | Running ExecutionInfo
  | Completed ExecutionInfo
  | -- | Task has failed with failure count
    Failed ExecutionInfo Int
  deriving (Show)

data TaskInfo
  = KnownTask TaskStatus
  | UnknownTask
  deriving (Show)

data ExecutionInfo = ExecutionInfo
  { _eiExecutor :: Executor,
    _eiElapsed :: TimeSpec
  }
  deriving (Show)

data TaskError
  = ExternalTaskFailed
      TaskDescription
      TaskInfo
      (Maybe (Path Abs File))
      (Maybe (Path Abs File))
  deriving (Show, Typeable)

instance Exception TaskError where
  displayException (ExternalTaskFailed td ti mbStdout mbStderr) =
    "External task failed to construct item '"
      ++ show (_tdOutput td)
      ++ "'. Task info: "
      ++ show ti
      ++ " stdout: "
      ++ show mbStdout
      ++ " stderr: "
      ++ show mbStderr
      ++ " Task: "
      ++ show (_tdTask td)

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
  popTask ::
    MonadIO m =>
    Hook c ->
    Executor ->
    m (Maybe TaskDescription)

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

startTask ::
  (Coordinator c, MonadIO m) =>
  Hook c ->
  m (Maybe TaskDescription)
startTask h = liftIO $ do
  executorInfo <- Executor <$> getHostName
  popTask h executorInfo

-- | Check if a task is currently 'in progress' - e.g.
--   pending or running.
isInProgress ::
  (Coordinator c, MonadIO m) =>
  Hook c ->
  ContentHash ->
  m Bool
isInProgress h ch = do
  ti <- taskInfo h ch
  return $ case ti of
    KnownTask Pending -> True
    KnownTask (Running _) -> True
    _ -> False

-- | Pop a task off of the queue for execution. Passes the popped task to the
--   given function for execution. If the function returns success ('Right'),
--   then the task will be marked as completed in the given time. If the
--   function returns failure ('Left'), then the task will be marked as
--   failed. If the function raises an exception or is interrupted by an
--   asynchronous exception, then the task will be placed back on the task
--   queue and the exception propagated. Returns 'Nothing' if no task is
--   available and @'Just' ()@ on task completion or regular failure.
withPopTask ::
  (Coordinator c, MonadIO m, MonadMask m, KatipContext m) =>
  Hook c ->
  Executor ->
  (TaskDescription -> m (TimeSpec, Either Int ())) ->
  m (Maybe ())
withPopTask hook executor f =
  bracketOnError
    (popTask hook executor)
    ( \case
        Nothing -> return ()
        Just td ->
          update td Pending
            `withException` \e ->
              $(logTM) ErrorS $
                "Failed to place task "
                  <> showLS (td ^. tdOutput)
                  <> " back on queue: "
                  <> ls (displayException (e :: SomeException))
    )
    ( \case
        Nothing -> return Nothing
        Just td ->
          f td >>= \case
            (t, Left ec) -> Just <$> update td (Failed (execInfo t) ec)
            (t, Right ()) -> Just <$> update td (Completed (execInfo t))
    )
  where
    update td = updateTaskStatus hook (td ^. tdOutput)
    execInfo = ExecutionInfo executor
