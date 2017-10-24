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

type PureCtx = M.Map T.Text ByteString
type NameSpace = ByteString

--sadly i seem unable to use a EitherT or ErrorT monad
newtype RFlowM a = RFlowM { runRFlowM :: StateT FlowST R.Redis a }
  deriving (Monad, Applicative, Functor, MonadIO, MonadState FlowST)

type FlowST = (Freshers, PureCtx, NameSpace)

nameForKey :: T.Text -> RFlowM ByteString
nameForKey k = do
  (_,_,ns) <- get
  return $ ns <> "_" <> DTE.encodeUtf8 k

rfresh :: RFlowM T.Text
rfresh = do
  (fs, ctx, ns) <- get
  let (f,nfs) = popFreshers fs
  put (nfs, ctx, ns)
  return f

fetchSym ::  Store a => T.Text -> RFlowM (Maybe a)
fetchSym k = do
  mv <- RFlowM . lift . R.get  =<< nameForKey k
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


rlookupSym :: Store a => T.Text -> RFlowM (Maybe a)
rlookupSym k = do
  (_, ctx, _) <- get
  case M.lookup k ctx of
    Nothing -> fetchSym k
    Just yv -> case decode yv of
                Right y -> return $ Just y
                Left _ -> fetchSym k

rputSym :: Store a => T.Text -> a -> RFlowM ()
rputSym k x = do
  _ <- RFlowM . lift . (`R.set` (encode x)) =<< nameForKey k
  putLocal k x

putLocal :: Store a => T.Text -> a -> RFlowM ()
putLocal k x = do
  (fs, ctx, ns) <- get
  put $ (fs, M.insert k (encode x) ctx, ns)

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
