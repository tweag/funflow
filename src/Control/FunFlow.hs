{-# LANGUAGE Arrows, GADTs, OverloadedStrings, TupleSections #-}

module Control.FunFlow where

import Control.FunFlow.Base

import Data.Aeson
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Control.Monad.State.Strict
import Data.Monoid ((<>))
import Control.Exception

collectNames :: Flow a b -> [T.Text]
collectNames (Name n f) = n : collectNames f
collectNames (Step _) = []
collectNames (Arr _) = []
collectNames (Compose f g) = collectNames f ++ collectNames g
collectNames (First f) = collectNames f

-- | a fresh variable supply
newtype Freshers = Freshers { unFreshers :: [T.Text] }

genFreshersPrefixed :: T.Text -> Freshers
genFreshersPrefixed p = Freshers $ map ((p<>) . T.pack . show) [(0::Int)..]

type PureCtx = M.Map T.Text Value

type FlowM a = StateT FlowST IO a

type FlowST = (Freshers, PureCtx)

initFlow :: FlowST
initFlow = (genFreshersPrefixed "", M.empty)

fresh :: FlowM T.Text
fresh = do
  (Freshers (f:fs), ctx) <- get
  put $ (Freshers fs, ctx)
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
puttingSym _ (Left s) = return $ Left s
puttingSym n (Right x) = putSym n x >> return (Right x)


runFlowM :: FlowM a -> IO  a
runFlowM fm =
   evalStateT fm initFlow


runFlow :: Flow a b -> a -> IO (Either (String, FlowST) b)
runFlow f ini = resumeFlow f ini initFlow

resumeFlow :: Flow a b -> a -> FlowST -> IO (Either (String, FlowST) b)
resumeFlow f ini flowst = do
  (ex, st) <- runStateT (proceedFlow f ini) flowst
  case ex of
    Left err -> return $ Left (err,st)
    Right x -> return $ Right x

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
      liftIO (fmap Right (f x) `catch` (\e -> return $ Left (show (e::SomeException))))

proceedFlow (Arr f) x = return $ Right $ f x
proceedFlow (Compose f g) x = do
  ey <- proceedFlow f x
  case ey of
    Left s -> return $ Left s
    Right y -> proceedFlow g y
proceedFlow (First f) (x,d) = do
  ey <- proceedFlow f x
  return $ fmap (,d) ey
