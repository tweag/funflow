{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Dedicated module for orphan instances.
module Control.Funflow.Orphans where

import           Data.Functor.Contravariant
import           Data.Store                 (Store)
import qualified Data.Store                 as Store
import qualified Path                       as Path
import qualified Path.Internal

instance Store (Path.Path Path.Abs Path.File) where
  size = contramap (\(Path.Internal.Path fp) -> fp) Store.size
  peek = Path.Internal.Path <$> Store.peek
  poke = Store.poke . (\(Path.Internal.Path fp) -> fp)
instance Store (Path.Path Path.Abs Path.Dir) where
  size = contramap (\(Path.Internal.Path fp) -> fp) Store.size
  peek = Path.Internal.Path <$> Store.peek
  poke = Store.poke . (\(Path.Internal.Path fp) -> fp)
instance Store (Path.Path Path.Rel Path.File) where
  size = contramap (\(Path.Internal.Path fp) -> fp) Store.size
  peek = Path.Internal.Path <$> Store.peek
  poke = Store.poke . (\(Path.Internal.Path fp) -> fp)
instance Store (Path.Path Path.Rel Path.Dir) where
  size = contramap (\(Path.Internal.Path fp) -> fp) Store.size
  peek = Path.Internal.Path <$> Store.peek
  poke = Store.poke . (\(Path.Internal.Path fp) -> fp)
