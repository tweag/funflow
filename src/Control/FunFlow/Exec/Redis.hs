{-# LANGUAGE Arrows, GADTs, OverloadedStrings, TupleSections, DeriveGeneric, StandaloneDeriving,
       TypeFamilies, GeneralizedNewtypeDeriving, ScopedTypeVariables, MultiParamTypeClasses  #-}

module Control.FunFlow.Exec.Redis where

import Control.FunFlow.Base
import Control.FunFlow

import Data.Store
import Data.Either (lefts, rights)
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as DTE
import Control.Monad.State.Strict
import Data.Monoid ((<>))
import Control.Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Database.Redis as R
import Control.Monad.Except
import GHC.Conc
import GHC.Generics
import Control.Concurrent.Async.Lifted
import Control.Monad.Trans.Control
import Control.Monad.Base
import Lens.Micro.Platform

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

data JobStatus = JobDone | JobError | JobRunning
  deriving (Generic, Show)

data Job a = Job
  { jobId :: JobId
  , taskName :: T.Text
  , jobStatus :: JobStatus
  , jobError :: Maybe String
  , argument :: a
  } deriving Generic

instance Store JobStatus
instance Store a => Store (Job a)

runRFlow :: R.Connection -> RFlowM a -> IO (Either String a)
runRFlow conn mx = do
  R.runRedis conn $ evalStateT (runExceptT mx) ("", conn)

redis ::  R.Redis (Either R.Reply a) -> RFlowM a
redis r = do ex <- lift $ lift r
             case ex of
               Left rply -> throwError $ "redis: "++show rply
               Right x -> return x

nameForKey :: T.Text -> RFlowM ByteString
nameForKey k = do
  (ns,_) <- get
  return $ ns <> "_" <> DTE.encodeUtf8 k

fresh :: RFlowM T.Text
fresh = do
  n <- redis $ R.incr "fffresh"
  return $ T.pack $ show (n::Integer)

lookupSym ::  Store a => T.Text -> RFlowM (Maybe a)
lookupSym k = do
  mv <- redis . R.get  =<< nameForKey k
  case mv of
    Just v -> return $ eitherToMaybe $ decode v
    _ -> return Nothing

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x

fromRight :: Show a => Either a b -> b
fromRight (Right x) = x
fromRight (Left e) = error $ "fromRight: Left "++show e

putSym :: Store a => T.Text -> a -> RFlowM ()
putSym k x = do
  _ <- redis .  (`R.set` (encode x)) =<< nameForKey k
  return ()


puttingSym :: Store a => T.Text -> a -> RFlowM a
puttingSym n x = putSym n x >> return x

redisPostOffice :: R.Connection -> PostOffice
redisPostOffice conn = PostOffice
  { reserveMailBox = R.runRedis conn $ do
      n <- fmap fromRight $ R.incr "pofresh"
      return $ MailBox $ T.pack $ show (n::Integer),
    send = \(MailBox nm) bs -> R.runRedis conn $ do
      void $ R.set (DTE.encodeUtf8 nm) bs,
    checkMail = \(MailBox nm) -> R.runRedis conn $ do
      fmap fromRight $ R.get (DTE.encodeUtf8 nm),
    awaitMail = \(MailBox nm) -> R.runRedis conn $ do
      waitGet (DTE.encodeUtf8 nm) -- temp until we do pubsub
  }
 where
  waitGet :: ByteString -> R.Redis ByteString
  waitGet k = do
    emv <- R.get k
    case emv of
      Left r -> fail $ "redis fail "++ show r
      Right (Nothing) -> do liftIO $ threadDelay 500000
                            waitGet k
      Right (Just v) -> return v


mdecode :: Store a => Maybe ByteString -> Either String a
mdecode (Nothing) = Left "no value"
mdecode (Just bs) = over _Left show $ decode bs

catching :: MonadError s m => m a -> m (Either s a)
catching mx =(fmap Right mx) `catchError` (\e -> return $ Left e)

sparkJob :: Store a => T.Text -> a -> RFlowM JobId
sparkJob nm x = do
  jid :: JobId <- redis $ R.incr "jobfresh"
  let job = Job jid nm JobRunning Nothing x
  redis $ R.rpush "jobs_running" [encode jid]
  redis $ R.set (BS8.pack $ "job_"++show jid) (encode job)
  return jid

getJobsByStatus :: JobStatus -> RFlowM [Job ()]
getJobsByStatus js = do
  let queueNm = case js of
                  JobRunning -> "jobs_running"
                  JobDone -> "jobs_done"
                  JobRunning -> "jobs_error"
  jids <- map decode <$> redis (R.lrange queueNm 0 (-1))
  mapM getJobStatus $ rights jids


resumeFirstJob :: forall a b. Store a => [(T.Text, Flow a b)] -> RFlowM ()
resumeFirstJob allJobs = do
  mjid <- redis $ R.lpop "jobs_running"
  case mdecode mjid of
    Left err -> return ()
    Right (jid::JobId) -> do
      let jobIdNm = BS8.pack $ "job_"++show jid
      mjob <- redis $ R.get jobIdNm
      case mdecode mjob of
        Left err -> return ()
        Right (job :: Job a) -> do
          case lookup (taskName job) allJobs of
            Nothing -> return ()
            Just flow -> do
              _1 .= jobIdNm
              finishJob job =<< catching (runJob flow (argument job))

finishJob :: Store a => Job a -> Either String b -> RFlowM ()
finishJob job y = do
  let jid = jobId job
  let jobIdNm = BS8.pack $ "job_"++show jid
  let newJob = case y of
         Right _ -> job {jobStatus = JobDone}
         Left err -> job {jobStatus = JobError, jobError = Just err}
  redis $ R.set jobIdNm (encode newJob)
  redis $ R.lrem "jobs_running" 1 (encode jid)
  case y of
    Right _ -> redis $ R.rpush "jobs_done" [encode jid]
    Left _ -> redis $ R.rpush "jobs_error" [encode jid]
  return ()

getJobStatus :: JobId -> RFlowM (Job ())
getJobStatus jid = do
  let jobIdNm = BS8.pack $ "job_"++show jid
  mjob <- redis $ R.get jobIdNm
  case mdecode mjob of
    Left err -> throwError err
    Right (job :: Job ()) -> return job

runJob :: Flow a b -> a -> RFlowM b
runJob (Step f) x = do
  n <- fresh
  mv <- lookupSym n
  case mv of
    Just y -> return y
    Nothing -> do
      ey <- liftIO $ fmap Right (f x)
                       `catch`
                         (\e -> return $ Left (show (e::SomeException)))
      case ey of
        Right y -> puttingSym n y
        Left err -> throwError err
runJob (Name n' f) x = do
  n <- (n'<>) <$> fresh
  mv <- lookupSym n
  case mv of
    Just y -> return y
    Nothing -> puttingSym n =<< runJob f x
runJob (Arr f) x = return $ f x
runJob (Compose f g) x = do
  y <- runJob f x
  runJob g y
runJob (First f) (x,d) = do
  ey <- runJob f x
  return $ (ey,d)
runJob (Fanin f _) (Left x) = do
  runJob f x
runJob (Fanin _ g) (Right x) = do
  runJob g x
runJob (Fold fstep) (lst,acc) = go lst acc where
  go [] y = return y
  go (x:xs) y0 = do
      y1 <- runJob fstep (x,y0)
      go xs y1
runJob (Catch f h) x = do
  --st <- get
  runJob f x `catchError` (\err -> do
    --put st  --TODO delete created variables?
    runJob h (x,err))
runJob (Par f g) (x,y) = do
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
