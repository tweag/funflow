{-# LANGUAGE Arrows, GADTs, OverloadedStrings, TupleSections, TypeFamilies #-}

module Control.FunFlow.Exec.Class where

import Control.FunFlow.Base

import Data.Store
import Data.Either (lefts)
import Data.List
import qualified Data.Text as T
import Control.Monad.State.Strict
import Data.Monoid ((<>))
import Control.Exception
import Control.Monad.IO.Class ()

class MonadIO m => FlowM m  where
  type FlowS m

  fresh :: m T.Text
  lookupSym :: Store a => T.Text -> m (Maybe a)
  putSym :: Store a => T.Text -> a -> m ()
  getState :: m (FlowS m)
  restoreState :: FlowS m -> m ()
  flowFail :: String -> m a
  flowCatch :: m a -> (String -> m a) -> m a

  par :: m a -> m b -> m (a,b)
  par mx my = liftM2 (,) mx my

puttingSym :: (FlowM m, Store a) => T.Text -> a -> m a
--puttingSym _ l@(Left _) = return l
puttingSym n x = putSym n x >> return x

proceedFlow :: FlowM m => Flow a b -> a -> m b
proceedFlow (Name n' f) x = do
  n <- (n'<>) <$> fresh
  mv <- lookupSym n
  case mv of
    Just y -> return y
    Nothing -> puttingSym n =<< proceedFlow f x

proceedFlow (Step f) x = do
  n <- fresh
  mv <- lookupSym n
  case mv of
    Just y -> return y
    Nothing -> do
      ey <- liftIO $ fmap Right (f x)
                       `catch`
                         (\e -> return $ Left (show (e::SomeException)))
      case ey of
        Right y -> puttingSym n y
        Left err -> flowFail err
proceedFlow (Arr f) x = return $ f x
proceedFlow (Compose f g) x = do
  y <- proceedFlow f x
  proceedFlow g y
proceedFlow (Par f g) (x,y) = do
  par (proceedFlow f x) (proceedFlow g y)
proceedFlow (First f) (x,d) = do
  ey <- proceedFlow f x
  return $ (ey,d)
proceedFlow (Fanin f _) (Left x) = do
  proceedFlow f x
proceedFlow (Fanin _ g) (Right x) = do
  proceedFlow g x
proceedFlow (Fold fstep) (lst,acc) = go lst acc where
  go [] y = return y
  go (x:xs) y0 = do
      y1 <- proceedFlow fstep (x,y0)
      go xs y1
proceedFlow (Catch f h) x = do
  st <- getState
  proceedFlow f x `flowCatch` (\err -> do
    restoreState st
    proceedFlow h (x,err))
