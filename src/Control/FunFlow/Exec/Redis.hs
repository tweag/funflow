{-# LANGUAGE Arrows, GADTs, OverloadedStrings, TupleSections #-}

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

type PureCtx = M.Map T.Text ByteString
type NameSpace = ByteString

--sadly i seem unable to use a EitherT or ErrorT monad
type FlowM a = StateT FlowST R.Redis a

type FlowST = (Freshers, PureCtx, NameSpace)

nameForKey :: T.Text -> FlowM ByteString
nameForKey k = return $ DTE.encodeUtf8 k

fresh :: FlowM T.Text
fresh = do
  (fs, ctx, ns) <- get
  let (f,nfs) = popFreshers fs
  put (nfs, ctx, ns)
  return f

fetchSym ::  Store a => T.Text -> FlowM (Maybe a)
fetchSym k = do
  mv <- lift . R.get =<< nameForKey k
  let process (Left _) = return Nothing
      process (Right x) = do
        putLocal k x
        return $ Just x
  case mv of
    Right (Just v) -> process $ decode v
    _ -> return Nothing

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x


lookupSym :: Store a => T.Text -> FlowM (Maybe a)
lookupSym k = do
  (_, ctx, _) <- get
  case M.lookup k ctx of
    Nothing -> fetchSym k
    Just yv -> case decode yv of
                Right y -> return $ Just y
                Left _ -> fetchSym k

putSym :: Store a => T.Text -> a -> FlowM ()
putSym k x = do
  _ <- lift . (`R.set` (encode x)) =<< nameForKey k
  putLocal k x

putLocal :: Store a => T.Text -> a -> FlowM ()
putLocal k x = do
  (fs, ctx, ns) <- get
  put $ (fs, M.insert k (encode x) ctx, ns)

puttingSym :: Store a => T.Text -> Either String a -> FlowM (Either String a)
puttingSym _ l@(Left _) = return l
puttingSym n r@(Right x) = putSym n x >> return r

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
proceedFlow :: Flow a b -> a -> FlowM (Either String b)
proceedFlow (Name n' f) x = do
  n <- (n'<>) <$> fresh
  mv <- lookupSym n
  case mv of
    Just y -> return $ Right y
    Nothing -> puttingSym n =<< proceedFlow f x

proceedFlow (Step f) x = do
  n <- fresh
  mv <- lookupSym n
  case mv of
    Just y -> return $ Right y
    Nothing -> do
      ey <- liftIO $ fmap Right (f x)
                       `catch`
                         (\e -> return $ Left (show (e::SomeException)))
      puttingSym n ey

proceedFlow (Arr f) x = return $ Right $ f x
proceedFlow (Compose f g) x = do
  ey <- proceedFlow f x
  case ey of
    Left s -> return $ Left s
    Right y -> proceedFlow g y
proceedFlow (Par f g) (x,y) = do
  ew <- proceedFlow f x
  ez <- proceedFlow g y
  case (ew, ez) of
    (Right w, Right z) -> return $ Right (w,z)
    _ -> return $ Left $ intercalate " and also " $ lefts [ew] ++ lefts [ez]
proceedFlow (First f) (x,d) = do
  ey <- proceedFlow f x
  return $ fmap (,d) ey
proceedFlow (Fanin f _) (Left x) = do
  proceedFlow f x
proceedFlow (Fanin _ g) (Right x) = do
  proceedFlow g x
proceedFlow (Fold fstep) (lst,acc) = go lst acc where
  go [] y = return $ Right y
  go (x:xs) y0 = do
      ey1 <- proceedFlow fstep (x,y0)
      case ey1 of
        Left err -> return $ Left err
        Right y1 -> go xs y1
proceedFlow (Catch f h) x = do
  st <- get
  ey <- proceedFlow f x
  case ey of
    Right y -> return $ Right y
    Left err -> put st >> proceedFlow h (x,err)