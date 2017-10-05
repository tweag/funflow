{-# LANGUAGE Arrows, GADTs, OverloadedStrings #-}

module Control.FunFlow.Steps where

import Control.FunFlow.Base
import Control.Arrow
import System.Random
import GHC.Conc (threadDelay)
import Data.Aeson

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

pauseWith :: (FromJSON a, ToJSON a) => Flow (Int, a) a
pauseWith = Step $ \(secs,a) -> do
  threadDelay (secs*1000000)
  return a


withBase :: a -> Flow (b,a) c -> Flow b c
withBase x f = proc y -> do
  z <- f -< (y,x)
  returnA -< z

mapF :: Flow a b -> Flow [a] [b]
mapF f = (withBase [] $ Fold $ proc (x,ys) -> do
      y <- f -< x
      returnA -< y:ys) >>> arr reverse

retry :: (FromJSON a, ToJSON a) => Int -> Int -> Flow a b -> Flow a b
retry 0 _ f = f
retry n secs f = Catch f $ proc (x,_) -> do
  x1 <- pauseWith -< (secs,x)
  x2 <- retry (n-1) secs f -< x1
  returnA -< x2
