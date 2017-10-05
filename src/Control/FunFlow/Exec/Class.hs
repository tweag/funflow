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

  par :: m a -> m b -> m (a,b)
  par mx my = liftM2 (,) mx my

puttingSym :: (FlowM m, Store a) => T.Text -> Either String a -> m (Either String a)
puttingSym _ l@(Left _) = return l
puttingSym n r@(Right x) = putSym n x >> return r

proceedFlow :: FlowM m => Flow a b -> a -> m (Either String b)
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
  (ew, ez) <- par (proceedFlow f x) (proceedFlow g y)
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
  st <- getState
  ey <- proceedFlow f x
  case ey of
    Right y -> return $ Right y
    Left err -> restoreState st >> proceedFlow h (x,err)