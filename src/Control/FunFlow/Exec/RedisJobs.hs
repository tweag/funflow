{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Control.FunFlow.Exec.RedisJobs where

import           Control.FunFlow.Exec.Redis
import           Control.FunFlow.External.Coordinator.Redis
import           Control.Lens                               hiding (argument)
import           Data.Either                                (rights)
import           Data.Maybe                                 (catMaybes)

import           Control.Exception
import           Control.FunFlow.Base
import           Control.FunFlow.Utils
import           Control.Monad.Except
import           Control.Monad.State.Strict
import qualified Data.ByteString.Char8                      as BS8
import           Data.Store
import qualified Data.Text                                  as T
import qualified Database.Redis                             as R
import           GHC.Generics

type JobId = Integer

data JobStatus
  = JobDone
  | JobError
  | JobRunning
  | JobQueue
  deriving (Generic, Show)

data Job a = Job
  { jobId     :: JobId
  , taskName  :: T.Text
  , jobStatus :: JobStatus
  , jobError  :: Maybe String
  , argument  :: a
  } deriving (Generic)

instance Store JobStatus

instance Store a => Store (Job a)

-- | Create a job in the waiting queue without actually running it
sparkJob :: Store a => T.Text -> a -> RFlowM JobId
sparkJob nm x = do
  jid :: JobId <- redis $ R.incr "jobfresh"
  let job = Job jid nm JobQueue Nothing x
  _ <- redis $ R.rpush "jobs_queue" [encode jid]
  _ <- redis $ R.set (BS8.pack $ "job_" ++ show jid) (encode job)
  return jid

-- | Get all the jobs by status
getJobsByStatus :: Store a => JobStatus -> RFlowM [Job a]
getJobsByStatus js = do
  let queueNm =
        case js of
          JobRunning -> "jobs_running"
          JobQueue   -> "jobs_queue"
          JobDone    -> "jobs_done"
          JobError   -> "jobs_error"
  jids <- map decode <$> redis (R.lrange queueNm 0 (-1))
  fmap catMaybes $ mapM getJobById $ rights jids

-- | Get a job by job ID
getJobById :: Store a => JobId -> RFlowM (Maybe (Job a))
getJobById jid = do
  let jobIdNm = BS8.pack $ "job_" ++ show jid
  mjob <- redis $ R.get jobIdNm
  case mdecode mjob of
    Left _err -> return Nothing
    Right job -> return $ Just job

-- | Loop forever, looking for new jobs that have been put on the waiting queue, and run them.
queueLoop ::
     forall a b eff ex. (Store a, Exception ex)
  => [(T.Text, Flow eff ex a b)]
  -> RFlowM ()
queueLoop allJobs = forever go
  where
    go = do
      mkj <- redis $ R.brpoplpush "jobs_queue" "job_running" 1
      whenRight (mdecode mkj) $ \jid -> do
        mjob <- getJobById jid
        --liftIO $ putStrLn $ "queueLoop got job id "++ show (jid,fmap jobId mjob)
        whenJust mjob $ resumeJob allJobs

runJobById ::
     forall a b eff ex. (Store a, Exception ex)
     => [(T.Text, Flow eff ex a b)]
     -> JobId
     -> RFlowM b
runJobById allJobs jid = do
  let jobIdNm = BS8.pack $ "job_" ++ show jid
  mjob <- redis $ R.get jobIdNm
  case mdecode mjob of
    Left _ -> throwError "no such job"
    Right job -> do
      conn <- snd <$> get
      case (lookup (taskName job) allJobs) of
        Just flow -> runJob Redis conn flow (argument job)
        Nothing   -> throwError "no such task"

-- | Resume a job
resumeJob ::
     forall a b eff ex. (Store a, Exception ex)
  => [(T.Text, Flow eff ex a b)]
  -> Job a
  -> RFlowM ()
resumeJob allJobs job = do
  --liftIO $ putStrLn $ "resumeJob got taskName "++ show (taskName job)
  whenJust (lookup (taskName job) allJobs) $ \flow -> do
    conn <- snd <$> get
    --liftIO $ putStrLn $ "resumeJob got job id "++ show (jobId job)
    let jobIdNm = BS8.pack $ "job_" ++ show (jobId job)
    _1 .= jobIdNm
    finishJob job =<< catching (runJob Redis conn flow (argument job))

-- | When a job has finished, mark it as done and put it on the done or error queues
finishJob :: Store a => Job a -> Either String b -> RFlowM ()
finishJob job y = do
  let jid = jobId job
  --liftIO $ putStrLn $ "finish got job id "++ show jid
  let jobIdNm = BS8.pack $ "job_" ++ show jid
  let newJob =
        case y of
          Right _  -> job {jobStatus = JobDone}
          Left err -> job {jobStatus = JobError, jobError = Just err}
  _ <- redis $ R.set jobIdNm (encode newJob)
  _ <- redis $ R.lrem "jobs_running" 1 (encode jid)
  _ <- case y of
    Right _ -> redis $ R.rpush "jobs_done" [encode jid]
    Left _  -> redis $ R.rpush "jobs_error" [encode jid]
  return ()
