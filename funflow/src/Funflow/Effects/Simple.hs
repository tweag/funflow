{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

{-
 - "Simple" effects allow to use pure functions or IO monadic continuations
 -}
module Funflow.Effects.Simple
  ( SimpleEffect (..),
  )
where

-- External effects to perform external tasks
data SimpleEffect i o where
  PureEffect :: (i -> o) -> SimpleEffect i o
  IOEffect :: (i -> IO o) -> SimpleEffect i o
