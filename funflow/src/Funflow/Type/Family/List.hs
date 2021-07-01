{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- TODO: use https://hackage.haskell.org/package/type-combinators
module Funflow.Type.Family.List where

type Ø = '[]

type (:<) = '(:)
infixr 5 :<


-- | Type-level singleton list.
type Only a = '[a]

type family Null (as :: [k]) :: Bool where
  Null Ø = 'True
  Null (a :< as) = 'False

-- | Appends two type-level lists.
type family (as :: [k]) ++ (bs :: [k]) :: [k] where
  Ø ++ bs = bs
  (a :< as) ++ bs = a :< (as ++ bs)

infixr 5 ++

type family Concat (ls :: [[k]]) :: [k] where
  Concat Ø = Ø
  Concat (l :< ls) = l ++ Concat ls