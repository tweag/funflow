-- | This modules defines the composition of an applicative functor and an
-- arrow, which is always an arrow.

module Control.Arrow.AppArrow
  ( AppArrow(..)
  , appArrow
  ) where

import Control.Category
import Control.Arrow
import Prelude          hiding (id, (.))

newtype AppArrow app arr a b = AppArrow { unAppArrow :: app (arr a b) }

instance (Applicative app, Category cat) => Category (AppArrow app cat) where
  id = appArrow id
  AppArrow a1 . AppArrow a2 = AppArrow $ (.) <$> a1 <*> a2

instance (Applicative app, Arrow arr) => Arrow (AppArrow app arr) where
  arr = appArrow . arr
  first (AppArrow a) = AppArrow $ first <$> a
  second (AppArrow a) = AppArrow $ second <$> a
  AppArrow a1 *** AppArrow a2 = AppArrow $ (***) <$> a1 <*> a2

instance (Applicative app, ArrowChoice arr) => ArrowChoice (AppArrow app arr) where
  left (AppArrow a) = AppArrow $ left <$> a
  right (AppArrow a) = AppArrow $ right <$> a
  AppArrow a1 +++ AppArrow a2 = AppArrow $ (+++) <$> a1 <*> a2
  AppArrow a1 ||| AppArrow a2 = AppArrow $ (|||) <$> a1 <*> a2

instance (Applicative f, Arrow arr) => Functor (AppArrow f arr t) where
  fmap f = (>>> arr f)

instance (Applicative app, Arrow arr) => Applicative (AppArrow app arr t) where
  pure = arr . const
  a <*> b = a &&& b >>> arr (uncurry ($))

appArrow :: (Applicative app) => arr a b -> AppArrow app arr a b
appArrow = AppArrow . pure
