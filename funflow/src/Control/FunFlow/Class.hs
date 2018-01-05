{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}
module Control.FunFlow.Class where

import           Control.Arrow
import           Control.Arrow.Free
import qualified Control.FunFlow.Base            as Base
import           Control.FunFlow.ContentHashable
import qualified Control.FunFlow.ContentStore    as CS
import           Control.FunFlow.External
import           Data.Default                    (def)
import           Path

class (ArrowChoice arr, ArrowError ex arr) => ArrowFlow eff ex arr | arr -> eff ex where
  -- | Create a flow from a pure function.
  step' :: Base.Properties a b -> (a -> b) -> arr a b
  -- | Create a flow from an IO action.
  stepIO' :: Base.Properties a b -> (a -> IO b) -> arr a b
  -- | Create an external task in the flow.
  external :: ContentHashable IO a => (a -> ExternalTask) -> arr a CS.Item
  -- | Create a flow from a user-defined effect.
  wrap' :: Base.Properties a b -> eff a b -> arr a b
  -- | Create a flow which will write its incoming data to the store.
  putInStore :: ContentHashable IO a => (Path Abs Dir -> a -> IO ()) -> arr a CS.Item
  -- | Create a flow which will read data from the given store item.
  getFromStore :: (Path Abs t -> IO a) -> arr (CS.Content t) a
  -- | Perform some internal manipulation of the content store.
  internalManipulateStore :: (CS.ContentStore -> a -> IO b) -> arr a b

instance ArrowFlow eff ex (Base.Flow eff ex) where
  step' props = effect . Base.Step props
  stepIO' props = effect . Base.StepIO props
  external = effect . Base.External
  wrap' p eff = effect $ Base.Wrapped p eff
  putInStore = effect . Base.PutInStore
  getFromStore = effect . Base.GetFromStore
  internalManipulateStore = effect . Base.InternalManipulateStore

-- | Create a flow from a pure function.
--   This is a variant on 'step'' which uses the default properties.
step :: ArrowFlow eff ex arr => (a -> b) -> arr a b
step = step' def

-- | Create a flow from an IO action.
--   This is a variant on 'stepIO'' which uses the default properties.
stepIO :: ArrowFlow eff ex arr => (a -> IO b) -> arr a b
stepIO = stepIO' def

wrap :: ArrowFlow eff ex arr => eff a b -> arr a b
wrap = wrap' def
