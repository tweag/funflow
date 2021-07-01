{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

import Control.Arrow
import Funflow
import Lib

generateSequence :: Flow () [Double]
generateSequence = pureFlow $ const [1, 2, 3, 4, 5]

flow :: Flow () ()
flow = proc _ -> do
  numbers <- generateSequence -< ()
  --- &&& is used to process tasks in parallel
  (total, count) <- pureFlow sum &&& pureFlow length -< numbers
  sequenceMean <- pureFlow calculateMean -< (total, fromIntegral count)
  ioFlow (print . msg) -< (numbers, sequenceMean)
  where
    calculateMean :: Fractional a => (a, a) -> a
    calculateMean (total, count) = total / count

    msg :: ([Double], Double) -> String
    msg (numbers, mean) = "Mean of " <> show numbers <> " is: " <> show mean

main :: IO ()
main = runFlow flow ()
