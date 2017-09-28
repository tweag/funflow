{-# LANGUAGE Arrows, GADTs #-}

module Control.FunFlow where

import Control.Arrow
import Control.Category
import Prelude hiding ((.))

data Flow a b where
  Step    :: (a -> IO b) -> Flow a b
  Name    :: String -> Flow a b -> Flow a b
  Compose :: Flow a b -> Flow b c -> Flow a c
  First   :: Flow b c -> Flow (b,d) (c,d)

instance Category Flow where
  id = Step return
  f . g = Compose g f

instance Arrow Flow where
  arr f = Step $ return . f
  first  = First

runFlow :: Flow a b -> a -> IO b
runFlow (Step f) x = f x
runFlow (Name _ f) x = runFlow f x
runFlow (Compose f g) x = do
  y <- runFlow f x
  runFlow g y
runFlow (First f) (x,d) = do
  y <- runFlow f x
  return (y,d)

promptFor :: Read a => Flow String a
promptFor = proc s -> do
     () <- Step putStr -< (s++"> ")
     s' <- Step (const getLine) -< ()
     returnA -< read s'

myFlow :: Flow () Bool
myFlow = proc () -> do
  age <- promptFor -< "How old are you"
  returnA -< age > 65
