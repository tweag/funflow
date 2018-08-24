{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE Arrows               #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE Strict               #-}
{-# LANGUAGE StrictData           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}


{-|
Module      : Vec
Description : A module for a length indexed vector with typical list utilities.

A module for a length indexed vector with typical list utilities.

-}


module Data.Vec where

-- External imports
import           Control.Arrow
import           Data.Hashable
    (Hashable)
import qualified Data.HashMap.Strict as M
import           Data.Kind
import           Data.Proxy

-- Internal imports
import           Data.Existentials
import           Data.Nat



-- # Data
--------------------------------------------------------------------------------

data Vec :: Nat -> Type -> Type where
  VNil :: Vec 'Z a
  (:.>) :: a -> Vec n a -> Vec ('S n) a

infixr 5 :.>
deriving instance Show a => Show (Vec n a)
instance Functor (Vec n) where
  fmap = vMap


-- | Existential Vectors
data SomeVec :: Type -> Type where
  SomeVec :: Vec n a -> SomeVec a

deriving instance Show a => Show (SomeVec a)

instance Functor SomeVec where
  fmap f (SomeVec v) = SomeVec $ fmap f v



-- * Functions
--------------------------------------------------------------------------------

-- ** Head & Tail

vHead :: Vec ('S n) a -> a
vHead (x :.> _) = x

vTail :: Vec ('S n) a -> Vec n a
vTail (_ :.> xs) = xs


-- * Conversions

toList :: Vec n a -> [a]
toList VNil       = []
toList (x :.> xs) = x : (toList xs)

fromList :: [a] -> SomeVec a
fromList [] = SomeVec VNil
fromList (x:xs) = case fromList xs of
  SomeVec v -> SomeVec $ x :.> v


-- * Miscellaneous utilities

-- | Vec Concat
vConcat :: Vec n a -> Vec m a -> Vec (n+m) a
vConcat VNil m       = m
vConcat (x :.> xs) m = x :.> (vConcat xs m)

-- | Vec Split
vSplit :: Vec n (a,b) -> (Vec n a, Vec n b)
vSplit VNil = (VNil, VNil)
vSplit ((a,b) :.> xs) = case vSplit xs of
  (as, bs) -> (a :.> as, b :.> bs)


-- | Find with Vec
vFind :: Eq a => Vec n a -> a -> Maybe (Some SNat)
vFind VNil _ = Nothing
vFind (x :.> xs) y =
  case x == y of
    True -> Just $ Some SZ
    False -> do
      Some n' <- vFind xs y
      return $ Some $ SS n'


-- * Vec Len

vLen :: Vec n a -> SNat n
vLen VNil       = SZ
vLen (_ :.> xs) = SS $ vLen xs

vLenProxy :: Vec n a -> Proxy n
vLenProxy _ = Proxy


-- * Vec Maps and Transforms

vMapA :: ArrowChoice arrow =>
  arrow a b ->
    (forall n. arrow (Vec n a) (Vec n b))
vMapA flow = proc vecA -> do
  case vecA of
    VNil -> returnA -< VNil
    (x :.> xs) -> do
      ys <- vMapA flow -<  xs
      y <- flow -< x
      returnA -< (y :.> ys)

-- | Maps in parallel
vMapA' :: ArrowChoice arrow =>
  arrow a b ->
    (forall n. arrow (Vec n a) (Vec n b))
vMapA' flow = proc vecA -> do
  case vecA of
    VNil -> returnA -< VNil
    (x :.> xs) -> do
      (y,ys) <- flow *** (vMapA' flow) -< (x,xs)
      returnA -< (y :.> ys)



vExA :: ArrowChoice arrow =>
  (forall n. arrow (Vec n a) (Vec n b)) ->
  arrow (SomeVec a) (SomeVec b)
vExA arrow = proc somevec -> do
  case somevec of
    SomeVec v -> do
      v' <- arrow -<  v
      returnA -< SomeVec v'



vMap :: (a -> b) -> Vec n a -> Vec n b
vMap _ VNil       = VNil
vMap f (x :.> xs) = f x :.> (vMap f xs)


-- | Transform a Vec with a Hashmap of bindings
vTransform :: (Eq a, Hashable a) => M.HashMap a b -> Vec n a -> Maybe (Vec n b)
vTransform _    VNil      = return VNil
vTransform map' (x :.> xs) = do
  x' <- M.lookup x map'
  xs' <- vTransform map' xs
  return $ x' :.> xs'


-- | Vec Sequencing
vSeq :: Monad m => Vec n (m a) -> m (Vec n a)
vSeq VNil = return VNil
vSeq (x :.> xs) = do
  y <- x
  ys <- vSeq xs
  return (y :.> ys)


