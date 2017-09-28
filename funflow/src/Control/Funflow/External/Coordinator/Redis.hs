{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Redis-based co-ordinator for Funflow.
--
--   There are two co-ordinators defined in this module. They differ in whether
--   they open a new connection to Redis or re-use an existing one. Other than
--   that they behave identically.
module Control.Funflow.External.Coordinator.Redis
  ( Redis (..)
  , RedisPreconnected (..)
  ) where

import           Control.Funflow.External
import           Control.Funflow.External.Coordinator
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Fix                    (fix)
import qualified Data.CAS.ContentHashable             as CHash
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

  submitTask conn td =
    liftIO $
      R.runRedis conn $ do
        void $ R.rpush "jobs_queue" [encode (jid, td ^. tdTask)]
        void $ R.set jid (encode Pending)
    where
      jid = CHash.toBytes $ td ^. tdOutput

  queueSize conn = liftIO $ R.runRedis conn $
    fromIntegral . either (const 0) id <$> R.llen "jobs_queue"

  taskInfo conn chash = liftIO $
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

  updateTaskStatus conn chash status = liftIO $
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
            Nothing    -> fail "Cannot decode content hash."

  dropTasks conn = liftIO . R.runRedis conn $ do
    job <- R.del ["jobs_queue"]
    case job of
      Left r -> fail $ "redis fail " ++ show r
      Right _ -> return ()


data RedisPreconnected = RedisPreconnected

newtype Preconnected = Preconnected R.Connection

-- | Allow a preestablished redis connection to be used.
instance Coordinator RedisPreconnected where
  type Config RedisPreconnected = R.Connection
  type Hook RedisPreconnected = Preconnected

  initialise = return . Preconnected

  submitTask (Preconnected conn) = submitTask conn
  queueSize (Preconnected conn) = queueSize conn
  taskInfo (Preconnected conn) = taskInfo conn
  awaitTask (Preconnected conn) = awaitTask conn
  updateTaskStatus (Preconnected conn) = updateTaskStatus conn
  popTask (Preconnected conn) = popTask conn
  dropTasks (Preconnected conn) = dropTasks conn
