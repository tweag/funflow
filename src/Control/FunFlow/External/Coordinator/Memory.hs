{-# LANGUAGE EmptyDataDecls  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
-- | In-memory co-ordinator for funflow. This module is not greatly useful
--   except for testing purposes.
module Control.FunFlow.External.Coordinator.Memory where

import           Control.Concurrent.STM.TVar
import           Control.FunFlow.External.Coordinator
import           Control.Lens
import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.STM
import qualified Data.Map.Strict                      as M
import           Data.UUID.V4                         (nextRandom)
import           System.Clock                         (fromNanoSecs)

data MemoryCoordinator

data MemHook = MemHook {
    _mhTaskQueue      :: TVar [(TaskId, TaskDescription)]
  , _mhExecutionQueue :: TVar (M.Map TaskId TaskInfo)
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
    tid <- TaskId <$> nextRandom
    atomically $
      modifyTVar (mh ^. mhTaskQueue) ((tid, td) : )
    return tid

  queueSize mh = liftIO $ do
    queue <- atomically . readTVar $ mh ^. mhTaskQueue
    return $ length queue

  taskInfo mh tid = liftIO $ do
    (eq, tq) <- atomically $ do
      eq <- readTVar (mh ^. mhExecutionQueue)
      tq <- readTVar (mh ^. mhTaskQueue)
      return (eq, tq)
    return $ case M.lookup tid eq of
      Just ti -> Just ti
      Nothing -> case M.lookup tid (M.fromList tq)  of
        Just td -> Just $ TaskInfo Pending (td ^. tdOutput)
        Nothing -> Nothing

  popTask mh executor = let
      executionInfo = ExecutionInfo executor (fromNanoSecs 0)
      mkTaskInfo td = TaskInfo (Running executionInfo) (td ^. tdOutput)
    in liftIO . atomically $ do
        tq <- readTVar (mh ^. mhTaskQueue)
        case reverse tq of
          [] -> return Nothing
          (x@(tid, td):xs) -> do
            writeTVar (mh ^. mhTaskQueue) xs
            modifyTVar (mh ^. mhExecutionQueue) $ \eq ->
              M.insert tid (mkTaskInfo td) eq
            return $ Just x

  updateTaskStatus mh tid stat = liftIO . atomically $ do
    modifyTVar (mh ^. mhExecutionQueue) $ \eq ->
      if M.member tid eq
      then M.adjust (tiStatus .~ stat) tid eq
      else error "Cannot update task status: task not executing."

