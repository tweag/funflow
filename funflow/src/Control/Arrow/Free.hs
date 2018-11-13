-- {-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE Arrows                #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

-- | Various varieties of free arrow constructions.
--
--   For all of these constructions, there are only two important functions:
--   - 'eval' evaluates the free arrow in the context of another arrow.
--   - 'effect' lifts the underlying effect into the arrow.
--
--   The class 'FreeArrowLike', which is not exported from this module, exists
--   to allow these to be defined generally.
--
--   This module also defines some arrow combinators which are not exposed by
--   the standard arrow library.
module Control.Arrow.Free
  ( Free
  , Choice
  , ErrorChoice
  , effect
  , eval
    -- * ArrowError
  , ArrowError(..)
  , catch
    -- * Arrow functions
  , mapA
  , mapSeqA
  , filterA
  , type (~>)
  ) where

import           Control.Arrow
import           Control.Arrow.AppArrow
import           Control.Category
import           Control.Exception.Safe (Exception, MonadCatch)
import qualified Control.Exception.Safe
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Writer
import qualified Control.Monad.Trans.Writer.Strict as SW
import           Data.Bool           (Bool)
import           Data.Constraint     (Constraint, Dict (..), mapDict, weaken1,
                                      weaken2)
import           Data.Either         (Either (..))
import           Data.Function       (const, flip, ($))
import           Data.List           (uncons)
import           Data.Maybe          (maybe)
import           Data.Monoid         (Monoid)
import           Data.Tuple          (uncurry)

-- | A natural transformation on type constructors of two arguments.
type x ~> y = forall a b. x a b -> y a b

--------------------------------------------------------------------------------
-- FreeArrowLike
--------------------------------------------------------------------------------

-- | Small class letting us define `eval` and `effect` generally over
--   multiple free structures
class FreeArrowLike fal where
  type Ctx fal :: (k -> k -> *) -> Constraint
  effect :: eff a b -> fal eff a b
  eval :: forall eff arr a b. ((Ctx fal) arr)
       => (eff ~> arr)
       -> fal eff a b
       -> arr a b

-- | Annoying hackery to let us tuple constraints and still use 'effect'
--   and 'eval'
class Join ( a :: k -> Constraint) (b :: k -> Constraint) (x :: k) where
  ctx :: Dict (a x, b x)
instance (a x, b x) => Join a b x where
  ctx = Dict

--------------------------------------------------------------------------------
-- Arrow
--------------------------------------------------------------------------------

-- | Freely generated arrows over an effect.
data Free eff a b where
  Pure :: (a -> b) -> Free eff a b
  Effect :: eff a b -> Free eff a b
  Seq :: Free eff a b -> Free eff b c -> Free eff a c
  Par :: Free eff a1 b1 -> Free eff a2 b2 -> Free eff (a1, a2) (b1, b2)

instance Category (Free eff) where
  id = Pure id
  (.) = flip Seq

instance Arrow (Free eff) where
  arr = Pure
  first f = Par f id
  second f = Par id f
  (***) = Par

instance FreeArrowLike Free where
  type Ctx Free = Arrow
  -- | Lift an effect into an arrow.
  effect :: eff a b -> Free eff a b
  effect = Effect

  -- | Evaluate given an implicit arrow
  eval :: forall eff arr a0 b0. (Arrow arr)
        => (eff ~> arr)
        -> Free eff a0 b0
        -> arr a0 b0
  eval exec = go
    where
      go :: forall a b. Free eff a b -> arr a b
      go freeA = case freeA of
          Pure f     -> arr f
          Seq f1 f2  -> go f2 . go f1
          Par f1 f2  -> go f1 *** go f2
          Effect eff -> exec eff

--------------------------------------------------------------------------------
-- ArrowChoice
--------------------------------------------------------------------------------

-- | Freely generated `ArrowChoice` over an effect.
newtype Choice eff a b = Choice {
  runChoice :: forall ac. ArrowChoice ac => (eff ~> ac) -> ac a b
}

instance Category (Choice eff) where
  id = Choice $ const id
  Choice f . Choice g = Choice $ \x -> f x . g x

instance Arrow (Choice eff) where
  arr a = Choice $ const $ arr a
  first (Choice a) = Choice $ \f -> first (a f)
  second (Choice a) = Choice $ \f -> second (a f)
  (Choice a) *** (Choice b) = Choice $ \f -> a f *** b f

instance ArrowChoice (Choice eff) where
  left (Choice a) = Choice $ \f -> left (a f)
  right (Choice a) = Choice $ \f -> right (a f)
  (Choice a) ||| (Choice b) = Choice $ \f -> a f ||| b f

instance FreeArrowLike Choice where
  type Ctx Choice = ArrowChoice
  effect :: eff a b -> Choice eff a b
  effect a = Choice $ \f -> f a

  eval :: forall eff arr a0 b0. (ArrowChoice arr)
       => (eff ~> arr)
       -> Choice eff a0 b0
       -> arr a0 b0
  eval f a = runChoice a f

--------------------------------------------------------------------------------
-- ErrorChoice
--------------------------------------------------------------------------------

-- | ArrowError represents those arrows which can catch exceptions within the
--   processing of the flow.
class ArrowError ex a where
  try :: a e c -> a e (Either ex c)

instance (ArrowError ex arr) => ArrowError ex (AppArrow (Reader r) arr) where
  try (AppArrow act) = AppArrow $ reader $ \r ->
    try $ runReader act r

instance (ArrowError ex arr, Monoid w) => ArrowError ex (AppArrow (Writer w) arr) where
  try (AppArrow act) = AppArrow $ writer (try arr, w)
    where (arr, w) = runWriter act

instance (ArrowError ex arr, Monoid w) => ArrowError ex (AppArrow (SW.Writer w) arr) where
  try (AppArrow act) = AppArrow $ SW.writer (try arr, w)
    where (arr, w) = SW.runWriter act

catch :: (ArrowError ex a, ArrowChoice a) => a e c -> a (e, ex) c -> a e c
catch a onExc = proc e -> do
  res <- try a -< e
  case res of
    Left ex ->
      onExc -< (e, ex)
    Right r ->
      returnA -< r

instance (Arrow (Kleisli m), Exception ex, MonadCatch m)
  => ArrowError ex (Kleisli m) where
    try (Kleisli a) = Kleisli $ Control.Exception.Safe.try . a

-- | Freely generated arrows with both choice and error handling.
newtype ErrorChoice ex eff a b = ErrorChoice {
  runErrorChoice :: forall ac. (ArrowChoice ac, ArrowError ex ac)
                 => (eff ~> ac) -> ac a b
}

instance Category (ErrorChoice ex eff) where
  id = ErrorChoice $ const id
  ErrorChoice f . ErrorChoice g = ErrorChoice $ \x -> f x . g x

instance Arrow (ErrorChoice ex eff) where
  arr a = ErrorChoice $ const $ arr a
  first (ErrorChoice a) = ErrorChoice $ \f -> first (a f)
  second (ErrorChoice a) = ErrorChoice $ \f -> second (a f)
  (ErrorChoice a) *** (ErrorChoice b) = ErrorChoice $ \f -> a f *** b f

instance ArrowChoice (ErrorChoice ex eff) where
  left (ErrorChoice a) = ErrorChoice $ \f -> left (a f)
  right (ErrorChoice a) = ErrorChoice $ \f -> right (a f)
  (ErrorChoice a) ||| (ErrorChoice b) = ErrorChoice $ \f -> a f ||| b f

instance ArrowError ex (ErrorChoice ex eff) where
  try (ErrorChoice a) = ErrorChoice $ \f -> try $ a f

instance FreeArrowLike (ErrorChoice ex) where
  type Ctx (ErrorChoice ex) = Join ArrowChoice (ArrowError ex)
  effect :: eff a b -> ErrorChoice ex eff a b
  effect a = ErrorChoice $ \f -> f a

  eval :: forall eff arr a0 b0. (Join ArrowChoice (ArrowError ex) arr)
       => (eff ~> arr)
       -> ErrorChoice ex eff a0 b0
       -> arr a0 b0
  eval f a = case (\x -> (mapDict weaken1 x, mapDict weaken2 x)) ctx of
    ( Dict :: Dict (ArrowChoice arr)
      , Dict :: Dict (ArrowError ex arr)
      ) -> runErrorChoice a f


--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

-- | Map an arrow over a list.
mapA :: ArrowChoice a => a b c -> a [b] [c]
mapA f = arr (maybe (Left ()) Right . uncons)
      >>> (arr (const []) ||| ((f *** mapA f) >>> arr (uncurry (:))))

-- | Map an arrow over a list, forcing sequencing between each element.
mapSeqA :: ArrowChoice a => a b c -> a [b] [c]
mapSeqA f = arr (maybe (Left ()) Right . uncons)
            >>> (arr (const []) ||| ((first f >>> second (mapSeqA f)) >>> arr (uncurry (:))))

-- | Filter a list given an arrow filter
filterA :: ArrowChoice a => a b Bool -> a [b] [b]
filterA f = proc xs ->
  case xs of
    [] -> returnA -< []
    (y:ys) -> do
      b <- f -< y
      if b then
        (second (filterA f) >>> arr (uncurry (:))) -< (y,ys)
      else
        filterA f -< ys


