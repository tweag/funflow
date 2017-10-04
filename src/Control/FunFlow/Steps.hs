{-# LANGUAGE Arrows, GADTs, OverloadedStrings #-}

module Control.FunFlow.Steps where

import Control.FunFlow.Base
import Control.Arrow
import System.Random

promptFor :: Read a => Flow String a
promptFor = proc s -> do
     () <- Step putStr -< (s++"> ")
     s' <- Step (const getLine) -< ()
     returnA -< read s'

worstBernoulli :: Flow Double Double
worstBernoulli = Step $ \p -> do
  r <- randomRIO (0,1)
  if r < p
    then return r
    else error $ "worstBernoulli fail with "++ show r++ " > "++show p