{-# LANGUAGE Arrows, GADTs, OverloadedStrings #-}

module Control.FunFlow.Steps where

import Control.FunFlow.Base
import Control.Arrow

promptFor :: Read a => Flow String a
promptFor = proc s -> do
     () <- Step putStr -< (s++"> ")
     s' <- Step (const getLine) -< ()
     returnA -< read s'