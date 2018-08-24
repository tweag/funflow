{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE Arrows               #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE Strict               #-}
{-# LANGUAGE StrictData           #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : HList
Description : A module for a heterogeneous list.

This is an internal module for a heterogenous list.  We chose to avoid the
existing HList module because its pattern match does not behave like a GADT
pattern match.  In addition to HList utilities, this module also includes a
proxy type for type level lists, 'ProxyList' and a fancy way to pattern match
on these type level lists with the class 'QEmpty'.

-}


module Data.HList where

-- External imports
import           Data.Kind
import           Data.Proxy
import           Data.Typeable
import           Prelude hiding
    ((&&))

-- Internal imports
import           Data.Existentials
import           Data.Nat



-- # Data
--------------------------------------------------------------------------------

data HList :: [Type] -> Type where
  HNil :: HList '[]
  (:>) :: a -> HList l -> HList (a:l)

infixr 5 :>
deriving instance LShowable l => Show (HList l)

-- | An existential 'HList'
data SomeHL where
  SomeHL :: LShowable l => HList l -> SomeHL

-- | A proxy type for a type level list
data ProxyList :: [k] -> Type where
  ProxyList :: ProxyList l


instance TraversibleL l => Show (ProxyList l) where
  show xs = proxyLShow xs


proxyLShow :: forall k (l :: [k]). (LTypeable l, QEmpty l) => ProxyList l -> String
proxyLShow xs =  case matchPL xs of
    EmptyL -> "[]"
    NonEmpty (y,ys) ->
      "(" ++ (show . typeRep $ y) ++ ")" ++ ":" ++ proxyLShow ys



-- Type Families
--------------------------------------------------------------------------------

type family AllTraversible (ls :: [[Type]]) :: Constraint where
  AllTraversible '[] = ()
  AllTraversible (l:ls) = (TraversibleL l, AllTraversible ls)

-- | A Traversable type level list is one where we can
-- pattern match on the list, inspect each type and print
-- an element of each type.
type family TraversibleL (l :: [Type]) :: Constraint where
  TraversibleL l = (LTypeable l, QEmpty l, LShowable l)

-- | Each element in the list is 'Typeable'.
type family LTypeable (l :: [k]) :: Constraint where
  LTypeable '[] = ()
  LTypeable (x:xs) = (Typeable x, LTypeable xs)

-- | Each element in the list has a 'Show' instance.
type family LShowable (l :: [Type]) :: Constraint where
  LShowable '[] = ()
  LShowable (x:xs) = (Show x, LShowable xs)


-- | Length of a type level list.
type family Length (x :: [k]) :: Nat where
  Length '[] = 'Z
  Length (x:xs) = 'S (Length xs)


-- | Type level index.
type family Index (l :: [Type]) (n :: Nat) :: (Maybe Type) where
  Index '[] n = 'Nothing
  Index (x:xs) 'Z = 'Just x
  Index (x:xs) ('S n') = Index xs n'


-- * Functions
---------------------------------------------------------

-- ** ProxyListCons
proxyListCons :: Proxy t -> ProxyList ls -> ProxyList (t:ls)
proxyListCons _ _ = ProxyList


-- ** Head & Tail

hHead :: (HList (l:ls)) -> l
hHead (x :> _) = x

hTail :: (HList (l:ls)) -> HList ls
hTail (_ :> xs) = xs

hProxy :: HList l -> ProxyList l
hProxy _ = ProxyList

-- ** Length

hLen :: HList l -> SNat (Length l)
hLen HNil      = SZ
hLen (_ :> xs) = SS (hLen xs)

xLen :: HList l -> Some SNat
xLen HNil = Some SZ
xLen (_ :> xs) = case xLen xs of
  Some n -> Some (SS n)


-- ** Indexing

data HIndex :: [Type] -> Type where
  HIndex :: Index l n ~ 'Just t =>
    ProxyList l -> TyProxy t -> SNat n -> HIndex l


hIndex :: Index l n ~ 'Just t => HList l -> SNat n -> t
hIndex (x :> _) SZ       = x
hIndex (_ :> xs) (SS n') = hIndex xs n'

hGetStr :: Index l n ~ 'Just String => HList l -> SNat n -> String
hGetStr l n = hIndex l n

-- ** Checking Types at Indexes

checkTypeAtIndex :: (Typeable t, LTypeable l, QEmpty l) =>
  SNat n -> Proxy t -> ProxyList l -> Maybe ('Just t :~: Index l n)
checkTypeAtIndex SZ (_ :: Proxy t) (ls :: ProxyList l) = case matchPL ls of
  EmptyL         -> Nothing
  NonEmpty (_,_) -> eqT :: Maybe ('Just t :~: Index l 'Z)
checkTypeAtIndex (SS n) p ls = case matchPL ls of
  EmptyL          -> Nothing
  NonEmpty (_,xs) -> checkTypeAtIndex n p xs



-- * A fancy way to pattern match on type level lists

-- | A data type which represents a pattern match on
-- a type level list.
data QueryEmpty :: forall k. [k] -> Type where
  EmptyL   :: QueryEmpty '[]
  NonEmpty :: forall (ls :: [k]) (l :: k).
    QEmpty ls => (Proxy l, ProxyList ls) -> QueryEmpty (l:ls)


-- | This class along with its two instances
-- ensure that any type level list has an instance
-- of this class. Hence, a user can pattern match on any
-- type level list.
class QEmpty l where
  matchPL :: ProxyList l -> QueryEmpty l

instance QEmpty '[] where
  matchPL _ = EmptyL

instance QEmpty ls => QEmpty (l:ls) where
  matchPL (_ :: ProxyList (l:ls)) =
    NonEmpty (Proxy :: Proxy l, ProxyList :: ProxyList ls)




