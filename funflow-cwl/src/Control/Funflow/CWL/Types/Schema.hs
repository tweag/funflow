{-# LANGUAGE Arrows               #-}
{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE Strict               #-}
{-# LANGUAGE StrictData           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances        #-}
{-# LANGUAGE FlexibleInstances        #-}

{-# OPTIONS_GHC -Wno-unused-top-binds   #-}

{-|
Module      : Schema
Description : A specification for a record.

This module provides a data type for a specification of a record,
a 'Schema' and several combinators and utilities.

-}


module Control.Funflow.CWL.Types.Schema
  ( Schema (..)
  , SomeSchema
  , SchemaSplit (..)
  , SchemaUnion (..)
  , SchemaReduction (..)
  , splitSchemas
  , unionSchemas
  , tryReduce
  , indexSchema
  , SchemaIndex (..)
  , proxyInList
  , proxyOutList
  , mapIds
  )
where

import Data.Kind
import Data.Proxy
import Data.Typeable
import Control.Funflow ( type (==>) )

-- Internal imports
import Data.HList
import Data.Vec
import Data.Nat
import Data.Existentials
import Data.ErrorMonad



-- * Schema Data Types
--------------------------------------------------------------------------------

-- | A 'Schema' is a row type. It is a specification for a record.
-- We specify the types of the fields by a @ProxyList@. We specify the names of
-- each of those fields with a list of strings of the same length as the
-- @ProxyList@. We do not, however, check that the field names we provide are
-- unique. We call these field names "ids".
data Schema :: [Type] -> Type where
  Schema :: TraversibleL l =>
    { sc_types :: ProxyList l
    , sc_ids :: Vec (Length l) String
    } -> Schema l

deriving instance Show (Schema l)


-- | A 'Field' is a (type,name) pair.
type Field t = (TyProxy t, String)

type SomeSchema = Some Schema


-- | A split of two schemas l l\' is some schema k
--   which can be converted into each.
--   We split the resulting schema to two schemas.
data SchemaSplit :: [Type] -> [Type] -> Type where
  SchemaSplit :: AllTraversible [l, l', k] =>
    { sc_split  :: Schema k
    , sc_splitL :: HList k -> HList l
    , sc_splitR :: HList k -> HList l'
    } -> SchemaSplit l l'

-- | A union of two schemas is some schema that
--   contains the fields of each (save repeats).
--   Theres a convertor from lists of the two
--   given schemas to the schema union
data SchemaUnion :: [Type] -> [Type] -> Type where
  SchemaUnion :: AllTraversible [l, l', k] =>
    { sc_union :: Schema k
    , sc_union_join :: (HList l, HList l') -> HList k
    } -> SchemaUnion l l'

-- | A 'SchemaReduction l l\'' is just a function of type
--   'HList l -> HList l\''.
data SchemaReduction :: [Type] -> [Type] -> Type where
  Reduction :: AllTraversible [l, l'] =>
    (HList l -> HList l') -> SchemaReduction l l'

instance Show (SchemaReduction l l') where
  show (Reduction (_ :: HList l -> HList l')) =
    show (ProxyList :: ProxyList l) ++ " && " ++
      show (ProxyList :: ProxyList l')



-- * Fundamental Combinators
--------------------------------------------------------------------------------

splitSchemas :: Schema l -> Schema l' -> SchemaSplit l l'
splitSchemas (Schema tys ids) s2@(Schema _ _) =
  case (matchPL tys,ids) of
    (EmptyL,VNil) ->  SchemaSplit s2 (const HNil) id
    (NonEmpty (t,ts), id' :.> ids') ->
      let recur = splitSchemas (Schema ts ids') s2 in
        add1Field (t,id') recur
  where

    add1Field :: (Typeable t, Show t) =>
      Field t -> SchemaSplit l1 l2 -> SchemaSplit (t:l1) l2
    add1Field field s = case s of
      SchemaSplit recurSchema tol1 tol2 ->
        case indexSchema recurSchema field of
          Just (SchemaIndex pf index) ->
            let indexIden = reIndex pf index in
            SchemaSplit recurSchema (indexIden tol1) tol2
          Nothing -> enlargeSplit field s


    reIndex :: AllTraversible [k, l] =>
      Index k n :~: 'Just t -> SNat n ->
      (HList k -> HList l) -> (HList k -> HList (t:l))
    reIndex Refl n f hlist = (hIndex hlist n) :> (f hlist)


    -- | Add one field to the (Schema k) and
    --   adjust the two splitting functions
    enlargeSplit :: (Typeable t, Show t) =>
      Field t -> SchemaSplit l l' -> SchemaSplit (t:l) l'
    enlargeSplit field (SchemaSplit s tol tol') = 
      SchemaSplit (addField field s) newTol newTol'
      where
        newTol  hlist = hHead hlist :> (tol $ hTail hlist)
        newTol' hlist = tol' $ hTail hlist


unionSchemas :: Schema l -> Schema l' -> SchemaUnion l l'
unionSchemas (Schema tys ids) s2@(Schema _ _) =
  case (matchPL tys, ids) of
    (EmptyL, VNil) -> SchemaUnion s2 snd
    (NonEmpty (t,ts), id' :.> ids') ->
      let recurSch = unionSchemas (Schema ts ids') s2 in
      union1Field (t,id') recurSch
  where

    union1Field :: (Typeable t, Show t) =>
      Field t -> SchemaUnion l l' -> SchemaUnion (t:l) l'
    union1Field field (SchemaUnion recurSch joiner) =
      case indexSchema recurSch field of
        Just _  ->
          SchemaUnion recurSch $ joiner . mapFst hTail
        Nothing ->
          SchemaUnion (addField field recurSch) newJoiner
      where
        newJoiner (h1,h2) = hHead h1 :> joiner (hTail h1,h2)


    mapFst :: (a -> b) -> (a,c) -> (b,c)
    mapFst f (a,c) = (f a, c)


tryReduce :: Schema l -> Schema l' -> ErrM (SchemaReduction l l')
tryReduce s1@(Schema _ _) (Schema tys ids) =
  case (matchPL tys, ids) of
    (EmptyL, VNil) ->
      return $ Reduction $ nilReduce s1
    (NonEmpty (t,ts), id' :.> ids') -> do
      Reduction red <- tryReduce s1 (Schema ts ids')
      let field = (t,id')
      let indexErr = ErrMsg $
            "Could not find the source: " ++ dispf field ++
            "\nAre you sure the name and type is correct?"
      let maybeIndex = indexSchema s1 field
      let errMIndex = injectMaybe maybeIndex indexErr
      SchemaIndex Refl index <- errMIndex
      let newReduction hl = (hIndex hl index) :> (red hl)
      return $ Reduction newReduction
  where
    nilReduce :: Schema l -> (HList l) -> HList '[]
    nilReduce _ _ = HNil

    dispf :: Typeable t => Field t -> String
    dispf (ty,name) = 
      "(name: "++ name ++
      ", type:"++ show (typeRep ty) ++" )"



-- * Basic Schema Operations
--------------------------------------------------------------------------------

mapIds :: forall l.
  (Vec (Length l) String -> Vec (Length l) String) -> Schema l -> Schema l
mapIds f (Schema p ids) = Schema p $ f ids


addField :: (Show t, Typeable t) => Field t -> Schema l -> Schema (t:l)
addField (_,name) (Schema _ names) =
  Schema ProxyList $ name :.> names


data SchemaIndex :: [Type] -> Type -> Type where
  SchemaIndex :: (TraversibleL l, Typeable t, Show t) =>
    { scIndexPf :: Index l n :~: 'Just t
    , scIndex :: SNat n
    } -> SchemaIndex l t

indexSchema :: (Typeable t, Show t) =>
  Schema l -> Field t -> Maybe (SchemaIndex l t)
indexSchema (Schema tys ids) field =
  case (matchPL tys, ids) of
    (EmptyL, VNil) -> Nothing
    (NonEmpty (t',ts'), n :.> ns) ->
      case fieldEq (t',n) field of
        Just Refl -> return $ SchemaIndex Refl SZ
        Nothing -> do
          SchemaIndex Refl index <- indexSchema (Schema ts' ns) field
          return $ SchemaIndex Refl (SS index)

  where
    fieldEq :: forall t t'. (Typeable t, Typeable t', Show t, Show t') =>
      Field t -> Field t' -> Maybe (t :~: t')
    fieldEq (_ :: Proxy t,s) (_ :: Proxy t',s') = 
      if   s == s'
      then eqT :: Maybe (t :~: t')
      else Nothing


-- * ProxyList Accessors
--------------------------------------------------------------------------------

proxyInList :: TraversibleL l => (HList l ==> b) -> ProxyList l
proxyInList _ = ProxyList

proxyOutList :: TraversibleL l' => (a ==> HList l') -> ProxyList l'
proxyOutList _ = ProxyList




