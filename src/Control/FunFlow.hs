{-# LANGUAGE Arrows, GADTs, OverloadedStrings #-}

module Control.FunFlow where

import Control.FunFlow.Base

import Control.Arrow
import Data.Aeson
import qualified Data.Map.Strict as M
import qualified Data.Text as T

promptFor :: Read a => Flow String a
promptFor = proc s -> do
     () <- Step putStr -< (s++"> ")
     s' <- Step (const getLine) -< ()
     returnA -< read s'

collectNames :: Flow a b -> [T.Text]
collectNames (Name n f) = n : collectNames f
collectNames (Step _) = []
collectNames (Arr _) = []
collectNames (Compose f g) = collectNames f ++ collectNames g
collectNames (First f) = collectNames f

type PureCtx = M.Map T.Text Value

resumeFlow :: PureCtx -> Flow a b -> a -> IO (PureCtx, b)
resumeFlow ctx (Name n f) x = case M.lookup n ctx of
  Nothing -> do (nctx, y) <- resumeFlow ctx f x
                return (M.insert n (toJSON y) nctx,y)
  Just yv -> do let Success y = fromJSON yv
                return (ctx, y)
