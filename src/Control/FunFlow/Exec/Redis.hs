{-# LANGUAGE Arrows, GADTs, OverloadedStrings, TupleSections, DeriveGeneric,
       TypeFamilies, GeneralizedNewtypeDeriving, ScopedTypeVariables  #-}

module Control.FunFlow.Exec.Redis where

import Control.FunFlow.Base
import Control.FunFlow

import Data.Store
import Data.Either (lefts)
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
import Lens.Micro

type NameSpace = ByteString

newtype RFlowM a = RFlowM { runRFlowM :: ExceptT String (StateT FlowST R.Redis) a }
  deriving (Monad, Applicative, Functor, MonadIO, MonadState FlowST, MonadError String)

type FlowST = (NameSpace)

type JobId = Integer

data JobStatus = JobDone | JobError String | JobRunning deriving Generic

data Job a = Job
  { jobId :: JobId
  , taskName :: T.Text
  , jobStatus :: JobStatus
  , argument :: a
  } deriving Generic

instance Store JobStatus
instance Store a => Store (Job a)

redis ::  R.Redis (Either R.Reply a) -> RFlowM a
redis r = do ex <- RFlowM $ lift $ lift r
             case ex of
               Left rply -> throwError $ "redis: "++show rply
               Right x -> return x

nameForKey :: T.Text -> RFlowM ByteString
nameForKey k = do
  (ns) <- get
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
  let job = Job jid nm JobRunning x
  redis $ R.rpush "jobs_running" [encode jid]
  redis $ R.set (BS8.pack $ "job_"++show jid) (encode job)
  return jid

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
              put jobIdNm -- set namespace
              res <- catching $ runJob flow (argument job)
              case res of
                Right _ -> redis $ R.set jobIdNm (encode (job {jobStatus = JobDone}))
                Left err -> redis $ R.set jobIdNm (encode (job {jobStatus = JobError err}))
              return ()
          return ()
  return ()

getJobStatus :: JobId -> RFlowM JobStatus
getJobStatus jid = do
  let jobIdNm = BS8.pack $ "job_"++show jid
  mjob <- redis $ R.get jobIdNm
  case mdecode mjob of
    Left err -> return $ JobError err
    Right (job :: Job ()) -> return $ jobStatus job


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
runJob (Par f g) (x,y) = do

    liftM2 (,) (runJob f x) (runJob g y)