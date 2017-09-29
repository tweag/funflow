{-# LANGUAGE Arrows, GADTs, OverloadedStrings #-}

module Control.FunFlow where

import Control.FunFlow.Base

import Data.Aeson
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Control.Monad.State.Strict
import Data.Monoid ((<>))

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

type FlowM a = StateT (Freshers, PureCtx) IO a

initFlow :: (Freshers, PureCtx)
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

puttingSym :: ToJSON a => T.Text -> a -> FlowM a
puttingSym n x = putSym n x >> return x


runFlowM :: FlowM a -> IO a
runFlowM fm = evalStateT fm initFlow

resumeFlow :: Flow a b -> a -> FlowM b
resumeFlow (Name n' f) x = do
  n <- (n'<>) <$> fresh
  mv <- lookupSym n
  case mv of
    Nothing -> puttingSym n =<< resumeFlow f x
    Just y -> return y
resumeFlow (Step f) x = do
  n <- fresh
  mv <- lookupSym n
  case mv of
    Nothing -> puttingSym n =<< lift (f x)
    Just y -> return y
resumeFlow (Arr f) x = return $ f x
resumeFlow (Compose f g) x = do
  y <- resumeFlow f x
  resumeFlow g y
resumeFlow (First f) (x,d) = do
  y <- resumeFlow f x
  return (y,d)
