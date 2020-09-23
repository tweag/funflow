{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

-- | Run a pure function or an IO monadic continuation
module Funflow.Tasks.Simple
  ( SimpleTask (..),
  )
where

data SimpleTask i o where
  PureTask :: (i -> o) -> SimpleTask i o
  IOTask :: (i -> IO o) -> SimpleTask i o
