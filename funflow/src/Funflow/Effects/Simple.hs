{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

-- | Run a pure function or an IO monadic continuation
module Funflow.Effects.Simple
  ( SimpleEffect (..),
  )
where

data SimpleEffect i o where
  PureEffect :: (i -> o) -> SimpleEffect i o
  IOEffect :: (i -> IO o) -> SimpleEffect i o
