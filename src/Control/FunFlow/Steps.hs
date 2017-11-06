{-# LANGUAGE Arrows              #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Control.FunFlow.Steps where

import           Control.Arrow
import           Control.Arrow.Free   (catch)
import           Control.Exception    (Exception, throw)
import           Control.FunFlow.Base
import           Data.Store
import           GHC.Conc             (threadDelay)
import           System.Random

promptFor :: Read a => Flow ex String a
promptFor = proc s -> do
     () <- step putStr -< (s++"> ")
     s' <- step (const getLine) -< ()
     returnA -< read s'

printS :: Show a => Flow ex a ()
printS = step $ \s-> print s

worstBernoulli :: Exception ex => (String -> ex) -> Flow ex Double Double
worstBernoulli errorC = step $ \p -> do
  r <- randomRIO (0,1)
  if r < p
    then return r
    else throw . errorC $ "worstBernoulli fail with "++ show r++ " > "++show p

-- | pause for a given number of seconds. Thread through a value to ensure
--   delay does not happen inparallel with other processing
pauseWith :: Store a => Flow ex (Int, a) a
pauseWith = step $ \(secs,a) -> do
  threadDelay (secs*1000000)
  return a

-- | `retry n s f` reruns `f` on failure at most n times with a delay of `s`
--   seconds between retries
retry :: forall ex a b. (Exception ex, Store a)
      => Int -> Int -> Flow ex a b -> Flow ex a b
retry 0 _ f = f
retry n secs f = catch f $ proc (x, (_ :: ex)) -> do
  x1 <- pauseWith -< (secs,x)
  x2 <- retry (n-1) secs f -< x1
  returnA -< x2
