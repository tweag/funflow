{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

-- | Run a pure function or an IO monadic continuation
module Funflow.Tasks.Store
  ( StoreTask (..),
  )
where

import Path (Abs, Dir, Path)
import Data.CAS.ContentStore as CS

data StoreTask i o where
  PutDir :: StoreTask (Path Abs Dir) CS.Item
  GetDir :: StoreTask (CS.Item) (Path Abs Dir)
