{-# LANGUAGE Arrows, GADTs, OverloadedStrings, TupleSections #-}

module Control.FunFlow.Steps where

import Control.FunFlow.Base
import Control.Arrow
import System.Random
import GHC.Conc (threadDelay)
import Data.Store

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


-- | pause for a given number of seconds. Thread through a value to ensure
--   delay does not happen inparallel with other processing
pauseWith :: Store a => Flow (Int, a) a
pauseWith = Step $ \(secs,a) -> do
  threadDelay (secs*1000000)
  return a

-- | Map a Flow over a list
mapF :: Flow a b -> Flow [a] [b]
mapF f = (,[]) ^>> (Fold $ proc (x,ys) -> do
      y <- f -< x
      returnA -< y:ys) >>> arr reverse
-- | Filter a list
filterF :: Flow a Bool -> Flow [a] [a]
filterF f = (,[]) ^>> (Fold $ proc (x,ys) -> do
      b <- f -< x
      returnA -< if b then x:ys else ys) >>> arr reverse

-- | `retry n s f` reruns `f` on failure at most n times with a delay of `s` seconds
-- between retries
retry :: (Store a) => Int -> Int -> Flow a b -> Flow a b
retry 0 _ f = f
retry n secs f = Catch f $ proc (x,_) -> do
  x1 <- pauseWith -< (secs,x)
  x2 <- retry (n-1) secs f -< x1
  returnA -< x2
