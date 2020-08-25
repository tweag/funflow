{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

-- | Run a pure function or an IO monadic continuation
module Funflow.Effects.Store
  ( StoreEffect (..),
  )
where

import Path (Abs, Dir, Path)
import Data.CAS.ContentStore as CS

data StoreEffect i o where
  PutDir :: StoreEffect (Path Abs Dir) CS.Item
  GetDir :: StoreEffect (CS.Item) (Path Abs Dir)
