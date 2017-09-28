{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Dedicated module for orphan instances.
module Data.CAS.StoreOrphans where

import           Data.Functor.Contravariant
import           Data.CAS.ContentHashable
import           Data.Store                 as Store
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

instance Store ContentHash where
  size = contramap toBytes size
  peek = fromBytes <$> peek >>= \case
    Nothing -> peekException "Store ContentHash: Illegal digest"
    Just x -> return x
  poke = poke . toBytes

instance Store ExternallyAssuredFile
instance Store ExternallyAssuredDirectory
