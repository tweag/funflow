{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Control.Arrow.Free
  ( Free
  , effect
  , evalA
  , Choice
  , effectChoice
  , evalChoice
  , mapA
  ) where

import           Control.Arrow
import           Control.Category
import           Data.Either      (Either (..))
import           Data.Function    (const, flip, ($))
import           Data.List        (uncons)
import           Data.Maybe       (maybe)
import           Data.Tuple       (uncurry)

type x ~> y = forall a b. x a b -> y a b

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

-- | Lift an effect into an arrow.
effect :: eff a b -> Free eff a b
effect = Effect

-- | Evaluate given an implicit arrow
evalA :: forall eff arr a0 b0. (Arrow arr)
      => (eff ~> arr)
      -> Free eff a0 b0
      -> arr a0 b0
evalA exec = go
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

effectChoice :: eff a b -> Choice eff a b
effectChoice a = Choice $ \f -> f a

evalChoice :: forall eff arr a0 b0. (ArrowChoice arr)
            => (eff ~> arr)
            -> Choice eff a0 b0
            -> arr a0 b0
evalChoice f a = runChoice a f

-- | Map an arrow over a list.
mapA :: ArrowChoice a => a b c -> a [b] [c]
mapA f = arr (maybe (Left ()) Right . uncons)
      >>> (arr (const []) ||| ((f *** mapA f) >>> arr (uncurry (:))))
