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

module Control.FunFlow.Exec.Redis where

import           Control.Arrow.Async
import           Control.Arrow.Free                   (eval)
import           Control.Exception
import           Control.FunFlow.Base
import qualified Control.FunFlow.ContentHashable      as CHash
import           Control.FunFlow.External
import           Control.FunFlow.External.Coordinator
import           Control.FunFlow.Utils
import           Control.Monad.Base
import           Control.Monad.Catch                  hiding (catch)
import qualified Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Fix                    (fix)
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Control
import           Data.ByteString                      (ByteString)
import           Data.Monoid                          ((<>))
import           Data.Store
import qualified Data.Text                            as T
import qualified Data.Text.Encoding                   as DTE
import qualified Database.Redis                       as R
import           GHC.Conc
import           Lens.Micro.Platform
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

type NameSpace = ByteString

type RFlowM a = ExceptT String (StateT FlowST R.Redis) a

instance MonadBase IO R.Redis where
  liftBase = liftIO

instance MonadBaseControl IO R.Redis where
  type StM R.Redis a = a
  liftBaseWith f = R.reRedis $ liftBaseWith $ \q -> f (q . R.unRedis)
  restoreM = R.reRedis . restoreM

instance MonadThrow R.Redis where
  throwM = liftIO . throwM

instance MonadCatch R.Redis where
  catch x y = R.reRedis
            $ Control.Monad.Catch.catch (R.unRedis x) (R.unRedis <$> y)

type FlowST = (NameSpace, R.Connection)

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

fresh :: RFlowM T.Text
fresh = do
  n <- redis $ R.incr "fffresh"
  return $ T.pack $ show (n :: Integer)

-- | Convert a key name to a redis key - using the current namespace
nameForKey :: T.Text -> RFlowM ByteString
nameForKey k = do
  (ns, _) <- get
  return $ ns <> "_" <> DTE.encodeUtf8 k

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
putSym n x = putSym_ n x >> return x

-- | The `Flow` arrow interpreter
runJob :: forall c ex a b. (Coordinator c, Exception ex)
        => c
        -> Config c
        -> Flow ex a b
        -> a
        -> RFlowM b
runJob _ cfg flow input = do
    hook <- initialise cfg
    runAsyncA (eval (runJob' hook) flow) input
  where
    runJob' :: Hook c -> Flow' a1 b1
            -> AsyncA (ExceptT String (StateT FlowST R.Redis)) a1 b1
    runJob' _ (Step f) = AsyncA $ \x -> do
      n <- fresh
      mv <- lookupSym n
      case mv of
        Just y -> return y
        Nothing -> do
          ey <-
            liftIO $
            fmap Right (f x) `catch`
            (\e -> return $ Left ((e :: SomeException)))
          case ey of
            Right y  -> putSym n y
            Left err -> throw err
    runJob' _ (Named n' f) = AsyncA $ \x -> do
      n <- (n' <>) <$> fresh
      mv <- lookupSym n
      case mv of
        Just y  -> return y
        Nothing -> putSym n $ f x
    runJob' po (External toTask) = AsyncA $ \x -> do
      chash <- liftIO $ CHash.contentHash (x, toTask x)
      submitTask po $ TaskDescription chash (toTask x)
      KnownTask _ <- awaitTask po chash
      return chash
