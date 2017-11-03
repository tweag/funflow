{-# LANGUAGE Arrows                     #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

module Control.FunFlow.Exec.Redis where

import           Control.FunFlow.Base

import           Data.Either                     (rights)
import           Data.Maybe                      (catMaybes)
import           Data.Store

import           Control.Concurrent.Async.Lifted
import           Control.Exception
import           Control.FunFlow.Utils
import           Control.Monad.Base
import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Control
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString.Char8           as BS8
import           Data.Monoid                     ((<>))
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as DTE
import qualified Database.Redis                  as R
import           GHC.Conc
import           GHC.Generics
import           Lens.Micro.Platform

type NameSpace = ByteString

type RFlowM a = ExceptT String (StateT FlowST R.Redis) a

instance MonadBase IO R.Redis where
  liftBase = liftIO

instance MonadBaseControl IO R.Redis where
  type StM R.Redis a = a
  liftBaseWith f = R.reRedis $ liftBaseWith $ \q -> f (q . R.unRedis)
  restoreM = R.reRedis . restoreM

type FlowST = (NameSpace, R.Connection)

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

-- | Run the RFlowM monad 
runRFlow :: R.Connection -> RFlowM a -> IO (Either String a)
runRFlow conn mx = do
  R.runRedis conn $ evalStateT (runExceptT mx) ("", conn)

-- | Use redis commands inside RFlowM
redis :: R.Redis (Either R.Reply a) -> RFlowM a
redis r = do
  ex <- lift $ lift r
  case ex of
    Left rply -> throwError $ "redis: " ++ show rply
    Right x   -> return x

-- | Convert a key name to a redis key - using the current namespace
nameForKey :: T.Text -> RFlowM ByteString
nameForKey k = do
  (ns, _) <- get
  return $ ns <> "_" <> DTE.encodeUtf8 k

-- | Get a fresh variable name
fresh :: RFlowM T.Text
fresh = do
  n <- redis $ R.incr "fffresh"
  return $ T.pack $ show (n :: Integer)

-- | Look up a value in the current namespace
lookupSym :: Store a => T.Text -> RFlowM (Maybe a)
lookupSym k = do
  mv <- redis . R.get =<< nameForKey k
  case mv of
    Just v -> return $ eitherToMaybe $ decode v
    _      -> return Nothing

-- | Store a value under a symbol in redis, discarding the result
putSym_ :: Store a => T.Text -> a -> RFlowM ()
putSym_ k x = do
  _ <- redis . (`R.set` (encode x)) =<< nameForKey k
  return ()

-- | Store a value under a symbol, and return it again
putSym :: Store a => T.Text -> a -> RFlowM a
putSym n x = putSym n x >> return x

-- | Create a `PostOffice` based on a redis database connection
redisPostOffice :: R.Connection -> PostOffice
redisPostOffice conn =
  PostOffice
  { reserveMailBox =
      R.runRedis conn $ do
        n <- fmap fromRight $ R.incr "pofresh"
        return $ MailBox $ T.pack $ show (n :: Integer)
  , send =
      \(MailBox nm) bs ->
        R.runRedis conn $ do void $ R.set (DTE.encodeUtf8 nm) bs
  , checkMail =
      \(MailBox nm) ->
        R.runRedis conn $ do fmap fromRight $ R.get (DTE.encodeUtf8 nm)
  , awaitMail =
      \(MailBox nm) ->
        R.runRedis conn $ do
          waitGet (DTE.encodeUtf8 nm) -- temp until we do pubsub
  }
  where
    waitGet :: ByteString -> R.Redis ByteString
    waitGet k = do
      emv <- R.get k
      case emv of
        Left r -> fail $ "redis fail " ++ show r
        Right (Nothing) -> do
          liftIO $ threadDelay 500000
          waitGet k
        Right (Just v) -> return v

-- | Create a job in the waiting queue without actually running it
sparkJob :: Store a => T.Text -> a -> RFlowM JobId
sparkJob nm x = do
  jid :: JobId <- redis $ R.incr "jobfresh"
  let job = Job jid nm JobQueue Nothing x
  redis $ R.rpush "jobs_queue" [encode jid]
  redis $ R.set (BS8.pack $ "job_" ++ show jid) (encode job)
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
    Left _    -> return Nothing
    Right job -> return $ Just job

-- | Loop forever, looking for new jobs that have been put on the waiting queue, and run them.
queueLoop ::
     forall a b. Store a
  => [(T.Text, Flow a b)]
  -> RFlowM ()
queueLoop allJobs = forever go
  where
    go = do
      mkj <- redis $ R.brpoplpush "jobs_queue" "job_running" 1
      whenRight (mdecode mkj) $ \jid -> do
        mjob <- getJobById jid
        whenJust mjob $ resumeJob allJobs

-- | Run the first job in the running queue. This is probably not so useful anymore
resumeFirstJob ::
     forall a b. Store a
  => [(T.Text, Flow a b)]
  -> RFlowM ()
resumeFirstJob allJobs = do
  mjid <- redis $ R.lpop "jobs_running"
  whenRight (mdecode mjid) $ \(jid :: JobId) -> do
    mjob <- getJobById jid
    whenJust mjob $ resumeJob allJobs

-- | Resume a job
resumeJob ::
     forall a b. Store a
  => [(T.Text, Flow a b)]
  -> Job a
  -> RFlowM ()
resumeJob allJobs job = do
  whenJust (lookup (taskName job) allJobs) $ \flow -> do
    let jobIdNm = BS8.pack $ "job_" ++ show (jobId job)
    _1 .= jobIdNm
    finishJob job =<< catching (runJob flow (argument job))

-- | When a job has finished, mark it as done and put it on the done or error queues
finishJob :: Store a => Job a -> Either String b -> RFlowM ()
finishJob job y = do
  let jid = jobId job
  let jobIdNm = BS8.pack $ "job_" ++ show jid
  let newJob =
        case y of
          Right _  -> job {jobStatus = JobDone}
          Left err -> job {jobStatus = JobError, jobError = Just err}
  redis $ R.set jobIdNm (encode newJob)
  redis $ R.lrem "jobs_running" 1 (encode jid)
  case y of
    Right _ -> redis $ R.rpush "jobs_done" [encode jid]
    Left _  -> redis $ R.rpush "jobs_error" [encode jid]
  return ()

-- | The `Flow` arrow interpreter
runJob :: Flow a b -> a -> RFlowM b
runJob (Step f) x = do
  n <- fresh
  mv <- lookupSym n
  case mv of
    Just y -> return y
    Nothing -> do
      ey <-
        liftIO $
        fmap Right (f x) `catch`
        (\e -> return $ Left (show (e :: SomeException)))
      case ey of
        Right y  -> putSym n y
        Left err -> throwError err
runJob (Name n' f) x = do
  n <- (n' <>) <$> fresh
  mv <- lookupSym n
  case mv of
    Just y  -> return y
    Nothing -> putSym n =<< runJob f x
runJob (Arr f) x = return $ f x
runJob (Compose f g) x = do
  y <- runJob f x
  runJob g y
runJob (First f) (x, d) = do
  ey <- runJob f x
  return $ (ey, d)
runJob (Fanin f _) (Left x) = do
  runJob f x
runJob (Fanin _ g) (Right x) = do
  runJob g x
runJob (Fold fstep) (lst, acc) = go lst acc
  where
    go [] y = return y
    go (x:xs) y0 = do
      y1 <- runJob fstep (x, y0)
      go xs y1
runJob (Catch f h) x
  --st <- get
 = do
  runJob f x `catchError`
    (\err
    --put st  --TODO delete created variables?
      -> do runJob h (x, err))
runJob (Par f g) (x, y) = do
  ax <- async (runJob f x)
  ay <- async (runJob g y)
  waitBoth ax ay
runJob (Async ext) x = do
  conn <- fmap snd get
  let po = redisPostOffice conn
  mbox <- liftIO $ reserveMailBox po
  liftIO $ ext x po mbox
  Right y <- decode <$> liftIO (awaitMail po mbox)
  return y
