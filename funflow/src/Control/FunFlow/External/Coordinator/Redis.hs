{-# LANGUAGE Arrows                     #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

module Control.FunFlow.External.Coordinator.Redis
  ( Redis (..)
  ) where

import qualified Control.FunFlow.ContentHashable      as CHash
import           Control.FunFlow.External
import           Control.FunFlow.External.Coordinator
import           Control.FunFlow.Utils
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Fix                    (fix)
import           Data.Store
import qualified Database.Redis                       as R
import           GHC.Conc
import           System.Clock                         (fromNanoSecs)

data Redis = Redis

instance Coordinator Redis where
  type Config Redis = R.ConnectInfo
  type Hook Redis = R.Connection

  -- | Create a redis connection
  initialise = liftIO . R.connect

  submitTask conn td = unlessM (isInProgress conn $ td ^. tdOutput) $
    liftIO $ do
      R.runRedis conn $ do
        void $ R.rpush "jobs_queue" [encode (jid, td ^. tdTask)]
        void $ R.set jid (encode Pending)
    where
      jid = CHash.toBytes $ td ^. tdOutput

  queueSize conn = liftIO $ R.runRedis conn $ do
    fromIntegral . fromRight 0 <$> R.llen "jobs_queue"

  taskInfo conn chash = liftIO $ do
    R.runRedis conn $ do
      eoutput <- R.get $ CHash.toBytes chash
      case eoutput of
        Left r -> fail $ "Redis fail: " ++ show r
        Right Nothing -> return UnknownTask
        Right (Just bs) -> case decode bs of
          Left r   -> fail $ "Decode fail: " ++ show r
          Right ti -> return $ KnownTask ti

  awaitTask conn chash = liftIO . R.runRedis conn $
    fix $ \waitGet -> do
      ti <- taskInfo conn chash
      case ti of
        UnknownTask -> return UnknownTask
        info@(KnownTask (Completed _)) -> return info
        info@(KnownTask (Failed _ _)) -> return info
        _ -> do
          liftIO $ threadDelay 500000
          waitGet

  updateTaskStatus conn chash status = liftIO $ do
    R.runRedis conn
      $ void $ R.set (CHash.toBytes chash) (encode status)

  popTask conn executor = liftIO . R.runRedis conn $ do
    job <- R.brpoplpush "jobs_queue" "job_running" 1
    case job of
      Left r -> fail $ "redis fail " ++ show r
      Right Nothing -> return Nothing
      Right (Just bs) -> case decode bs of
        Left r                          -> fail $ "Decode fail: " ++ show r
        Right (chashbytes, task) ->
          case CHash.fromBytes chashbytes of
            Just chash -> do
              let status = Running $ ExecutionInfo executor (fromNanoSecs 0)
              _ <- R.set chashbytes (encode status)
              return . Just $ TaskDescription chash task
            Nothing    -> fail $ "Cannot decode content hash."
