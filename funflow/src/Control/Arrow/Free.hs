-- {-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE Arrows                #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
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
{-# LANGUAGE DerivingVia           #-}

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
  , TryEffect
  , catch
    -- * Arrow functions
  , mapA
  , mapSeqA
  , filterA
  , type (~>)
  ) where

import           Control.Arrow
import           Control.Category
import           Control.Kernmantle.Error hiding (catch)
import           Control.Kernmantle.Rope hiding (type (~>))
import           Data.Bool              (Bool)
import           Data.Either            (Either (..))
import           Data.Function          (const, flip)
import           Data.List              (uncons)
import           Data.Maybe             (maybe)
import           Data.Tuple             (uncurry)


-- | A natural transformation on type constructors of two arguments.
type x ~> y = forall a b. x a b -> y a b

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

-- | Lift an effect into a Free arrow.
effect :: eff a b -> Free eff a b
effect = Effect

-- | Evaluate Free given an implicit arrow
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

type Choice eff a b =
  AnyRopeWith '[ '( "userEffect", eff ) ] '[ArrowChoice] a b

--------------------------------------------------------------------------------
-- ErrorChoice
--------------------------------------------------------------------------------

catch :: (TryEffect ex a, ArrowChoice a) => a e c -> a (e, ex) c -> a e c
catch a onExc = proc e -> do
  res <- tryE a -< e
  case res of
    Left ex ->
      onExc -< (e, ex)
    Right r ->
      returnA -< r

type ErrorChoice ex eff a b =
  AnyRopeWith '[ '( "userEffect", eff ) ] '[TryEffect ex, ArrowChoice] a b

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


