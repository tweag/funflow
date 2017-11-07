{-# LANGUAGE EmptyDataDecls  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
-- | In-memory co-ordinator for funflow. This module is not greatly useful
--   except for testing purposes.
module Control.FunFlow.External.Coordinator.Memory where

import           Control.Concurrent                   (threadDelay)
import           Control.Concurrent.STM.TVar
import           Control.FunFlow.ContentHashable      (ContentHash)
import           Control.FunFlow.External
import           Control.FunFlow.External.Coordinator
import           Control.Lens
import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.STM
import           Data.List                            (find)
import qualified Data.Map.Strict                      as M
import           System.Clock                         (fromNanoSecs)

data MemoryCoordinator = MemoryCoordinator

data MemHook = MemHook {
    _mhTaskQueue      :: TVar [TaskDescription]
  , _mhExecutionQueue :: TVar (M.Map ContentHash TaskStatus)
  }

makeLenses ''MemHook

instance Coordinator MemoryCoordinator where
  type Config MemoryCoordinator = ()
  type Hook MemoryCoordinator = MemHook

  initialise () = liftIO . atomically $ do
    taskQueue <- newTVar mempty
    executionQueue <- newTVar mempty
    return $ MemHook taskQueue executionQueue

  submitTask mh td = liftIO $ do
    atomically $
      modifyTVar (mh ^. mhTaskQueue) (td : )

  queueSize mh = liftIO $ do
    queue <- atomically . readTVar $ mh ^. mhTaskQueue
    return $ length queue

  taskInfo mh tid = liftIO $ do
    (eq, tq) <- atomically $ do
      eq <- readTVar (mh ^. mhExecutionQueue)
      tq <- readTVar (mh ^. mhTaskQueue)
      return (eq, tq)
    return $ case M.lookup tid eq of
      Just ti -> KnownTask ti
      Nothing -> case find ((==tid) . (^. tdOutput)) tq of
        Just _  -> KnownTask Pending
        Nothing -> UnknownTask

  awaitTask mh tid = liftIO $ do
    ti <- taskInfo mh tid
    case ti of
      UnknownTask -> return UnknownTask
      info@(KnownTask (Completed _)) -> return info
      info@(KnownTask (Failed _ _)) -> return info
      _ -> do
        threadDelay 1000000
        awaitTask mh tid

  popTask mh executor = let
      executionInfo = ExecutionInfo executor (fromNanoSecs 0)
      taskStatus = Running executionInfo
    in liftIO . atomically $ do
        tq <- readTVar (mh ^. mhTaskQueue)
        case reverse tq of
          [] -> return Nothing
          (td:xs) -> do
            writeTVar (mh ^. mhTaskQueue) xs
            modifyTVar (mh ^. mhExecutionQueue) $ \eq ->
              M.insert (td ^. tdOutput) taskStatus eq
            return $ Just td

  updateTaskStatus mh tid stat = liftIO . atomically $ do
    modifyTVar (mh ^. mhExecutionQueue) $ \eq ->
      if M.member tid eq
      then M.insert tid stat eq
      else error "Cannot update task status: task not executing."

