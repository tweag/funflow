{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}

-- | Defines a class for types which can be used as workflows.
--
--   In general, you should not need this functionality unless you are defining
--   your own flow types (perhaps to use a state transformer or similar), in
--   which case this allows you to work using the standard combinators in
--   Funflow and then define the mapping to 'Base.Flow'
--
--   For an example of use, see the module @Control.Funflow.Checkpoints@, which
--   defines a checkpointed flow.
module Control.Funflow.Class where

import           Control.Arrow
import           Control.Arrow.AppArrow
import           Control.Arrow.Free
import qualified Control.Funflow.Base            as Base
import           Control.Funflow.ContentHashable
import qualified Control.Funflow.ContentStore    as CS
import           Control.Funflow.External
import           Data.Default                    (def)
import           Path

class (Arrow arr, ArrowError ex arr) => ArrowFlow eff ex arr | arr -> eff ex where
  -- | Create a flow from a pure function.
  step' :: Base.Properties a b -> (a -> b) -> arr a b
  -- | Create a flow from an IO action.
  stepIO' :: Base.Properties a b -> (a -> IO b) -> arr a b
  -- | Create an external task in the flow.
  external :: (a -> ExternalTask) -> arr a CS.Item
  -- | Create an external task with additional properties
  external' :: Base.ExternalProperties a -> (a -> ExternalTask) -> arr a CS.Item
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
  external = effect . Base.External def
  external' p td = effect $ Base.External p td
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

instance ( Applicative app
         , ArrowError ex (AppArrow app (arr eff ex))
         , ArrowFlow eff ex (arr eff ex) )
      => ArrowFlow eff ex (AppArrow app (arr eff ex)) where
  step' props f = appArrow $ step' props f
  stepIO' props f = appArrow $ stepIO' props f
  external f = appArrow $ external f
  external' props f = appArrow $ external' props f
  wrap' props eff = appArrow $ wrap' props eff
  putInStore f = appArrow $ putInStore f
  getFromStore f = appArrow $ getFromStore f
  internalManipulateStore f = appArrow $ internalManipulateStore f
