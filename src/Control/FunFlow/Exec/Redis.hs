{-# LANGUAGE Arrows, GADTs, OverloadedStrings, TupleSections,
       TypeFamilies, GeneralizedNewtypeDeriving #-}

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
import qualified Database.Redis as R
import GHC.Conc

type NameSpace = ByteString

newtype RFlowM a = RFlowM { runRFlowM :: StateT FlowST R.Redis a }
  deriving (Monad, Applicative, Functor, MonadIO, MonadState FlowST)

type FlowST = (NameSpace)

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
      n <- fmap fromRight $ R.incr "fffresh"
      return $ MailBox $ T.pack $ show (n::Integer),
    send = \(MailBox nm) bs -> R.runRedis conn $ do
      void $ R.set (DTE.encodeUtf8 nm) bs,
    checkMail = \(MailBox nm) -> R.runRedis conn $ do
      fmap fromRight $ R.get (DTE.encodeUtf8 nm),
    awaitMail = \(MailBox nm) -> R.runRedis conn $ do
      waitGet (DTE.encodeUtf8 nm) -- temp until we do pubsub
  }

waitGet :: ByteString -> R.Redis ByteString
waitGet k = do
  emv <- R.get k
  case emv of
    Left r -> fail $ "redis fail "++ show r
    Right (Nothing) -> do liftIO $ threadDelay 500000
                          waitGet k
    Right (Just v) -> return v



{-
runTillDone :: Flow a b -> a -> IO b
runTillDone f x = go M.empty where
  go st0 = do
    ey <- resumeFlow f x st0
    case ey of
      Right y -> return y
      Left (err, st1) -> do putStrLn $ "Flow failed with "++err
                            go st1

resumeFlow :: Flow a b -> a -> PureCtx -> IO (Either (String, PureCtx) b)
resumeFlow f ini ctx = do
  (ex, st) <- runStateT (proceedFlow f ini) (initFreshers, ctx)
  case ex of
    Left err -> return $ Left (err,snd st)
    Right x ->  return $ Right x
-}
