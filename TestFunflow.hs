{-# LANGUAGE Arrows, GADTs, OverloadedStrings #-}

import Control.FunFlow.Base
import Control.FunFlow
import Control.Arrow


myFlow :: Flow () Bool
myFlow = proc () -> do
  age <- promptFor <: "getNm" -< "How old are you"
  returnA -< age > (65::Int)

main :: IO ()
main = do res <- runFlow myFlow ()
          print res