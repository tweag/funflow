{-# LANGUAGE Arrows, GADTs, OverloadedStrings, TupleSections,
             TypeFamilies, GeneralizedNewtypeDeriving #-}

module Control.FunFlow.Exec.Local where

import Control.FunFlow.Base
import Control.FunFlow
import Control.FunFlow.Exec.Class
import Data.Store
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Control.Monad.State.Strict
import Data.ByteString (ByteString)

instance FlowM LFlowM where
  type FlowS LFlowM = FlowST
  fresh = lfresh
  lookupSym = llookupSym
  putSym = lputSym
  getState = get
  restoreState = put

type PureCtx = M.Map T.Text ByteString

--sadly i seem unable to use a EitherT or ErrorT monad
newtype LFlowM a = LFLowM { runLFlowM :: StateT FlowST IO a }
  deriving (Monad, Applicative, Functor, MonadIO, MonadState FlowST)

type FlowST = (Freshers, PureCtx)

lfresh :: LFlowM T.Text
lfresh = do
  (fs, ctx) <- get
  let (f,nfs) = popFreshers fs
  put (nfs, ctx)
  return f

llookupSym :: Store a => T.Text -> LFlowM (Maybe a)
llookupSym k = do
  (_, ctx) <- get
  case M.lookup k ctx of
    Nothing -> return Nothing
    Just yv -> case decode yv of
                Right y -> return $ Just y
                Left _ -> return Nothing

lputSym :: Store a => T.Text -> a -> LFlowM ()
lputSym k x = do
  (fs, ctx) <- get
  put $ (fs, M.insert k (encode x) ctx)


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
  (ex, st) <- runStateT (runLFlowM $ proceedFlow f ini) (initFreshers, ctx)
  case ex of
    Left err -> return $ Left (err,snd st)
    Right x ->  return $ Right x
