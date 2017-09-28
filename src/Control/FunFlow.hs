{-# LANGUAGE Arrows, GADTs, OverloadedStrings #-}

module Control.FunFlow where

import Control.Arrow
import Control.Category
import Prelude hiding ((.), id)
import qualified Prelude
import Data.Aeson
import qualified Data.Map.Strict as M
import qualified Data.Text as T

data Flow a b where
  Step    :: (a -> IO b) -> Flow a b
  Arr     :: (a -> b) -> Flow a b
  Name    :: (FromJSON a,FromJSON b, ToJSON b) => T.Text -> Flow a b -> Flow a b
  Compose :: Flow a b -> Flow b c -> Flow a c
  First   :: Flow b c -> Flow (b,d) (c,d)

instance Category Flow where
  id = Arr Prelude.id
  f . g = Compose g f

instance Arrow Flow where
  arr f = Arr f
  first  = First

(<:) :: (FromJSON a,FromJSON b, ToJSON b) => Flow a b -> T.Text -> Flow a b
f <: nm = Name nm f

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
  age <- promptFor <: "getNm" -< "How old are you"
  returnA -< age > (65::Int)

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
