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
import GHC.Conc
import GHC.Generics
import Lens.Micro

type NameSpace = ByteString

newtype RFlowM a = RFlowM { runRFlowM :: StateT FlowST R.Redis a }
  deriving (Monad, Applicative, Functor, MonadIO, MonadState FlowST)

type FlowST = (NameSpace)

type JobId = Integer

data JobStatus = JobDone | JobError String | JobRunning deriving Generic

data Job a = Job
  { jobId :: JobId
  , taskName :: T.Text
  , argument :: a
  , jobStatus :: JobStatus
  } deriving Generic

instance Store JobStatus
instance Store a => Store (Job a)

redis ::  R.Redis (Either R.Reply a) -> RFlowM a
redis r = do ex <- RFlowM $ lift r
             case ex of
               Left rply -> fail $ "redis: "++show rply
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
  _ <- RFlowM . lift . (`R.set` (encode x)) =<< nameForKey k
  return ()

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

sparkJob :: Store a => T.Text -> a -> RFlowM JobId
sparkJob nm x = do
  jid :: JobId <- redis $ R.incr "jobfresh"
  let job = Job jid nm x JobRunning
  redis $ R.rpush "jobs_running" [encode jid]
  redis $ R.set (BS8.pack $ "job_"++show jid) (encode job)
  return jid

resumeJobs :: forall a b. Store a => [(T.Text, Flow a b)] -> RFlowM ()
resumeJobs allJobs = do
  mjid <- redis $ R.lpop "jobs_running"
  case mdecode mjid of
    Left err -> return ()
    Right (jid::JobId) -> do
      mjob <- redis $ R.get (BS8.pack $ "job_"++show jid)
      case mdecode mjob of
        Left err -> return ()
        Right (job :: Job a) -> do
          --lookup job
          --set namespace
          --runJob
          return ()
  return ()

getJobStatus :: Store b => JobId -> RFlowM (Either String b)
getJobStatus jid = do
  return undefined

runJob :: Flow a b -> a -> RFlowM ()
runJob flow x = do
  return ()

mdecode :: Store a => Maybe ByteString -> Either String a
mdecode (Nothing) = Left "no value"
mdecode (Just bs) = over _Left show $ decode bs