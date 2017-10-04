{-# LANGUAGE Arrows, GADTs, OverloadedStrings, TupleSections #-}

module Control.FunFlow.Exec.Local where

import Control.FunFlow.Base
import Control.FunFlow

import Data.Aeson
import Data.Either (lefts)
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Control.Monad.State.Strict
import Data.Monoid ((<>))
import Control.Exception


type PureCtx = M.Map T.Text Value

--sadly i seem unable to use a EitherT or ErrorT monad
type FlowM a = StateT FlowST IO a

type FlowST = (Freshers, PureCtx)

fresh :: FlowM T.Text
fresh = do
  (fs, ctx) <- get
  let (f,nfs) = popFreshers fs
  put (nfs, ctx)
  return f

withFreshesPre :: T.Text -> FlowM a -> FlowM a
withFreshesPre pre fm = do
  (oldfs, ctx) <- get
  let newfs = genFreshersPrefixed pre
  put $ (newfs, ctx)
  res <- fm
  put $ (oldfs, ctx)
  return res

lookupSym :: FromJSON a => T.Text -> FlowM (Maybe a)
lookupSym k = do
  (_, ctx) <- get
  case M.lookup k ctx of
    Nothing -> return Nothing
    Just yv -> case fromJSON yv of
                Success y -> return $ Just y
                Error _ -> return Nothing

putSym :: ToJSON a => T.Text -> a -> FlowM ()
putSym k x = do
  (fs, ctx) <- get
  put $ (fs, M.insert k (toJSON x) ctx)

puttingSym :: ToJSON a => T.Text -> Either String a -> FlowM (Either String a)
puttingSym _ l@(Left _) = return l
puttingSym n r@(Right x) = putSym n x >> return r

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
