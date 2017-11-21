{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

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
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Control
import           Data.ByteString                      (ByteString)
import           Data.Monoid                          ((<>))
import           Data.Store
import qualified Data.Text                            as T
import qualified Data.Text.Encoding                   as DTE
import qualified Database.Redis                       as R

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
runJob :: forall c eff ex a b. (Coordinator c, Exception ex)
        => c
        -> Hook c
        -> Flow eff ex a b
        -> a
        -> RFlowM b
runJob _ hook flow input = do
    runAsyncA (eval (runJob' hook) flow) input
  where
    runJob' :: Hook c -> Flow' eff a1 b1
            -> AsyncA (ExceptT String (StateT FlowST R.Redis)) a1 b1
    runJob' _ (StepIO _ f) = AsyncA $ \x -> liftIO $ f x
      -- n <- fresh
      -- mv <- lookupSym n
      -- case mv of
      --   Just y -> return y
      --   Nothing -> do
      --     ey <-
      --       liftIO $
      --       fmap Right (f x) `catch`
      --       (\e -> return $ Left (e::SomeException) )
      --     case ey of
      --       Right y  -> putSym n y
      --       Left err -> do
      --         throwError $ show err
    runJob' _ (Step (name -> Just n') f) = AsyncA $ \x -> return $ f x
      -- n <- (n' <>) <$> fresh
      -- mv <- lookupSym n
      -- case mv of
      --   Just y  -> return y
      --   Nothing -> putSym n $ f x
    runJob' po (External toTask) = AsyncA $ \x -> do
      chash <- liftIO $ CHash.contentHash (x, toTask x)
      submitTask po $ TaskDescription chash (toTask x)
      KnownTask _ <- awaitTask po chash
      -- XXX: Make Redis executor store aware
      undefined
