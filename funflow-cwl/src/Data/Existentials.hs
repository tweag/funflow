{-# LANGUAGE GADTs      #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeInType #-}

{-|
Module      : Existentials
Description : This exports some generic existential types.
-}


module Data.Existentials where

import           Data.Kind
import           Data.Proxy
import           Data.Typeable


-- | A generic existential type for one-indexed types.
-- For example, (Some SNat) is an existential SNat.
data Some :: (a -> Type) -> Type where
  Some :: k a -> Some k

type SomeProxy = Some Proxy

type TyProxy (t :: Type) = Proxy t

-- | This is a proxy for a 'Typeable' type
data SomeTyProxy where
  SomeTyProxy ::
    (Typeable t, Show t) =>
    TyProxy t -> SomeTyProxy
