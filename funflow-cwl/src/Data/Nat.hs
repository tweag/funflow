{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE Strict               #-}
{-# LANGUAGE StrictData           #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}


{-|
Module      : Nat
Description : A module for a peano 'Nat' data type with basic utilities.

A module for a peano 'Nat' data type with basic utilities.
-}


module Data.Nat where

-- External imports
import           Data.Kind



-- * Data definitions
--------------------------------------------------------------------------------
data Nat where
  Z :: Nat
  S :: Nat -> Nat

data SNat :: Nat -> Type where
  SZ :: SNat 'Z
  SS :: SNat n -> SNat ('S n)

-- * Type Families
--------------------------------------------------------------------------------

type family (+) (x :: Nat) (y :: Nat) :: Nat where
  'Z + m = m
  ('S n) + m = 'S (n + m)

type family Sum (ns :: [Nat]) :: Nat where
  Sum '[] = 'Z
  Sum (x:xs) = x + (Sum xs)

