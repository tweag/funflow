{-# LANGUAGE Arrows, GADTs, OverloadedStrings, TupleSections,
             TypeFamilies, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}

module Control.FunFlow.Exec.Local where

import Control.FunFlow.Base
import Control.FunFlow
import Control.FunFlow.Exec.Class
import Data.Store
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Control.Monad.State.Strict
import Data.ByteString (ByteString)
import Control.Monad.Except

instance FlowM LFlowM where
  type FlowS LFlowM = FlowST
  fresh = lfresh
  lookupSym = llookupSym
  putSym = lputSym
  getState = get
  restoreState = put

type PureCtx = M.Map T.Text ByteString

--sadly i seem unable to use a EitherT or ErrorT monad
newtype LFlowM a = LFLowM { runLFlowM :: ExceptT String (StateT FlowST IO) a }
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

resumeFlow :: forall a b. Flow a b -> a -> PureCtx -> IO (Either (String, PureCtx) b)
resumeFlow f ini ctx = do
  (eres, st) <- runStateT (runExceptT $ runLFlowM $ proceedFlow f ini) (initFreshers, ctx)
  case eres of
    Left err ->  return $ Left (err, snd st)
    Right res -> return $ Right res
