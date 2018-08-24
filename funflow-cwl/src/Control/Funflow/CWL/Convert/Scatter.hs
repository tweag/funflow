{-# LANGUAGE Arrows              #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict              #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeInType          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeApplications    #-}


{-|
Module      : Scatter
Description : Scattering a 'SomeFlow' to make some inputs lists.

This module exports 'scatter' which scatters a 'SomeFlow'. Given a list of
fields to scatter, it makes those fields lists and produces an 
HList where each output is a list of what it previously was.
So, informally,

@scatter ["field1"]
{SomeFlow [("field1",Int) ("field2",Bool)] ==> [("out",String)]}@

is

@SomeFlow [("field1", [Int]), ("field2",Bool)] ==> [("out",[String])]@.

-}

module Control.Funflow.CWL.Convert.Scatter ( scatter ) where



-- External
import Data.Kind
import Data.Proxy
import Data.Typeable
import qualified Data.Set as S
import Control.Arrow
import Control.Funflow ( step )


import           Data.HList
import           Data.ErrorMonad
import Data.Existentials
import Data.Vec
import Data.Nat
import Data.Type.Equality
import           Control.Funflow.CWL.Types.FunflowTypes
import           Control.Funflow.CWL.Types.CWLTypes
  ( Scatter (..),  ScatterMethod (..) )
import           Control.Funflow.CWL.Types.Schema



-- * Scatter Combinator


-- ** Type Level Scattering

-- | Given a type level list, and a boolean list of the same length, 
-- make the types t which correspond to 'True' into lists [t].
type family ScatterList (l :: [Type]) (bs :: [Bool]) :: [Type] where
  ScatterList '[] _ = '[]
  ScatterList l '[] = l
  ScatterList (l:ls) (b:bs) = (ScatterTy l b) : (ScatterList ls bs)

-- | Given a boolean, determine if t should be [t].
type family ScatterTy (t :: Type) (b :: Bool) :: Type where
  ScatterTy t 'True = [t]
  ScatterTy t 'False = t

-- | Convert all types in a list, into lists of themselves
type family ScatterAll (l :: [Type]) :: [Type] where
  ScatterAll '[] = '[]
  ScatterAll (t:ts) = ([t]) : (ScatterAll ts)

-- | A type level list of booleans
type BoolList (bs :: [Bool]) = ProxyList bs

-- A BoolList of some fixed length, that can be pattern matched on
-- both as a list, and then, each element as a boolean.
data SomeBoolList (n :: Nat) where
  SomeBL :: (Length bs ~ n, QEmpty bs, AllMatchable bs) =>
    BoolList bs -> SomeBoolList n


-- ** Untyped Scatter Function

-- | Given a SomeFlow and a Scatter, tranform the input flow to have
-- some inputs be lists of the original type. Then, the new flow should
-- creates a list of jobs via the modified inputs through some list
-- operation like a dot product and feed those jobs into the original flow.
scatter :: SomeFlow -> Scatter -> ErrM SomeFlow
scatter (SomeFlow flow inSch outSch) (Scatter toScat method) = do
  let inIds = sc_ids inSch
  let outIds = sc_ids outSch
  let flowinL = flowInProxy flow
  let flowoutL = flowOutProxy flow

  SomeBL bs <- determineScatter inIds toScat
  Dict <- return $ travScatLPf flowinL bs
  Dict <- return $ travScatAPf flowoutL
  Refl <- return $ scatterLenPf flowinL bs
  Refl <- return $ scatterAllLenPf flowoutL

  let inSch' = Schema (scatPL flowinL bs) inIds
  let outSch' = Schema (scatAllPL flowoutL) outIds
  case method of
    DotScat  -> do
      let flow' = dotScatter bs flow
      return $ SomeFlow flow' inSch' outSch'
    CrossScat -> do
      let flow' = crossScatter bs flow
      return $ SomeFlow flow' inSch' outSch'
  where

    -- Given a list of ids and a list of ids to scatter, create a type
    -- level list determining which inputs should be scattered.
    determineScatter :: Vec n String -> [String] -> ErrM (SomeBoolList n)
    determineScatter ids toScat' = do
      let err = ErrMsg $ "Repeated inputs in scatter: " ++ show toScat'
      let toScatter = S.fromList toScat'
      guardErrM (S.size toScatter == length toScat') err
      detScatter ids toScatter

    detScatter :: Vec n String -> S.Set String -> ErrM (SomeBoolList n)
    detScatter VNil _            = return $ SomeBL proxyNil
    detScatter (x :.> xs) toScat' = do
      SomeBL bs <- detScatter xs toScat'
      case S.member x toScat' of
        False -> return $ SomeBL $ falseCons bs
        True  -> return $ SomeBL $ trueCons bs

    proxyNil :: ProxyList '[]
    proxyNil = ProxyList

    trueCons :: BoolList bs -> BoolList ('True:bs)
    trueCons _ = ProxyList

    falseCons :: BoolList bs -> BoolList ('False:bs)
    falseCons _ = ProxyList

    scatPL :: ProxyList l -> BoolList bs -> ProxyList (ScatterList l bs)
    scatPL _ _ = ProxyList

    scatAllPL :: ProxyList l -> ProxyList (ScatterAll l)
    scatAllPL _ = ProxyList





-- ** Typed Scatter Function

-- | This is some function that takes a HList where some inputs
-- have been turned into lists and creates a Vec of "jobs", that is, a
-- Vec of the original HList.
data JobMaker bs l where
  JobMaker :: Length bs ~ Length l =>
    (AJobMaker bs l) -> JobMaker bs l

-- | Take some proxies, then an HList of items to scatter, and then make
-- a vector of HLists.
type AJobMaker bs l =
  (ProxyList l, BoolList bs) -> HList (ScatterList l bs) -> SomeVec (HList l)


-- | A typed version of 'scatter' that takes as input a JobMaker.
scatterFn :: forall l l' bs.
  ( AllTraversible [l,l', ScatterList l bs, ScatterAll l']
  , Length bs ~ Length l, AllMatchable bs, QEmpty bs) =>
  JobMaker bs l ->
  BoolList bs ->
  (HList l ==> HList l') ->
  (HList (ScatterList l bs) ==> HList (ScatterAll l'))
scatterFn (JobMaker jobMaker) bs flow = proc scatList -> do
  let flowIn = flowInProxy flow
  vecJobs <- (arr id) -<  jobMaker (flowIn, bs) scatList
  vecOuts <- vExA $ vMapA' flow -< vecJobs
  outList <- step toOutHList -<  vecOuts
  returnA -< outList


-- *** The two specific forms of scattering

-- | Take the dot product of "listified" inputs to make the
-- jobs. Like 'zip', defer to the smallest list:
-- Say we start with [Int, Bool, Int] and the type level transformation
-- yields [ Int, [Bool], [Int] ]. Then, for input
--
-- [2, [True, False, False], [ 3,4]] should produce two jobs:
-- (1) [2, True, 3]
-- (2) [2, False, 4]
--
dotScatter :: forall l l' bs.
 ( AllTraversible [l,l', ScatterList l bs, ScatterAll l']
 , AllMatchable bs
 , Length bs ~ Length l
 , QEmpty bs ) =>
 BoolList bs ->
 (HList l ==> HList l') ->
 (HList (ScatterList l bs) ==> HList (ScatterAll l'))
dotScatter = scatterFn $ JobMaker $ mkJobMaker dotList


-- Take the cross product of "listified" inputs to produce jobs.
-- With the example from 'dotScatter', we would produce six lists:
--
-- (1) [2, True, 3]
-- (2) [2, True, 4]
-- (3) [2, False, 3]
-- (4) [2, False, 4]
-- (5) [2, False, 3]
-- (6) [2, False, 4]
--
crossScatter :: forall l l' bs.
  ( AllTraversible [l,l', ScatterList l bs, ScatterAll l']
  , AllMatchable bs
  , Length bs ~ Length l
  , QEmpty bs ) =>
  BoolList bs -> (HList l ==> HList l') ->
  (HList (ScatterList l bs) ==> HList (ScatterAll l'))
crossScatter = scatterFn $ JobMaker $ mkJobMaker crossList



-- *** Scattering Using List Combinators

-- | Given a list of one input @[a]@, and a list of jobs @Vec n (HList l)@,
-- produce a new list of jobs @SomeVec (HList (a:l))@ with the input @a@ at the
-- front.
type ListCombinator =
  forall l a n. QEmpty l => [a] -> Vec n (HList l) -> SomeVec (HList (a:l))

-- | Given a list combinator, proxies to determine which inputs are "listified"
-- and a list of inputs, some of which have been "listified", produce a list of
-- jobs.
mkJobMaker :: forall l bs.
  (AllMatchable bs, QEmpty bs, QEmpty l, Length bs ~ Length l) =>
  ListCombinator ->
  (ProxyList l, BoolList bs) -> HList (ScatterList l bs) -> SomeVec (HList l)
mkJobMaker comb (pL,bL) hl = case (matchPL pL, matchPL bL) of
  (EmptyL, EmptyL) -> SomeVec (HNil :.> VNil)
  (NonEmpty (_,(ys :: ProxyList ls)), NonEmpty (b,bs)) -> case hl of
    (h :> hs) -> case mkJobMaker @ls comb (ys,bs) hs of
      SomeVec scattered -> case matchBool b of
        SFalse -> SomeVec $ vMap (h :>) scattered
        STrue ->  comb h scattered



-- **** Two List combinators

-- | Given a list of one input and a list of jobs, make a new list of jobs
-- using each @a@ exactly once.
dotList :: forall l a n.
  QEmpty l => [a] -> Vec n (HList l) -> SomeVec (HList (a:l))
dotList [] _ = SomeVec VNil
dotList xs v@VNil = case matchPL (getProxyL v) of
  EmptyL -> fmap (:> HNil) $ fromList xs
  NonEmpty _ -> SomeVec VNil
dotList (a:as) (x :.> xs) = case dotList as xs of
  SomeVec dottedXS -> SomeVec $ (a :> x) :.> dottedXS

-- | Given a list of one input and a list of jobs, make a new list of jobs
-- trying all pairs of each input @a@ with each job.
crossList ::
  [a] -> Vec n (HList l) -> SomeVec (HList (a:l))
crossList [] _   = SomeVec VNil
crossList _ VNil = SomeVec VNil
crossList (a:as) hs = case crossList as hs of
  SomeVec crossedList ->
      SomeVec $ vConcat (vMap (a :>) hs) crossedList

getProxyL :: Vec n (HList l) -> ProxyList l
getProxyL _ = ProxyList





-- ** List Format Conversion

-- | Make a list of output HLists, a HList of a list of each type.
toOutHList :: QEmpty l => SomeVec (HList l) -> HList (ScatterAll l)
toOutHList (SomeVec v) = toOutHList' v

toOutHList' :: QEmpty l =>
   Vec n (HList l) -> HList (ScatterAll l)
toOutHList' v@VNil = case matchPL (getProxyL v) of
  EmptyL -> HNil
  NonEmpty (_,xs) -> [] :> toOutHList' (makeNil xs)

  where
    makeNil :: ProxyList l -> Vec 'Z (HList l)
    makeNil _ = VNil

toOutHList' (x :.> xs) = addTopLayer x (toOutHList' xs)
  where
    addTopLayer :: HList l -> HList (ScatterAll l) -> HList (ScatterAll l)
    addTopLayer HNil HNil = HNil
    addTopLayer (x' :> xs') (ys :> yss) = (x':ys) :> (addTopLayer xs' yss)


-- ** Proofs


-- *** Proofs about preserving constraints after scattering

-- TODO: remove not important proxies with synonyms

travScatLPf ::
  ( TraversibleL l, QEmpty bs
  , Length bs ~ Length l, AllMatchable bs
  ) => ProxyList l -> BoolList bs -> Dict (TraversibleL (ScatterList l bs))
travScatLPf p bs = case (matchPL p, matchPL bs) of
  (EmptyL, EmptyL) -> Dict
  (NonEmpty (x,xs), NonEmpty (y,ys)) ->
    case travScatLPf xs ys of
      Dict ->
        case travOneTy x y of
          Dict -> Dict
  where

    travOneTy :: (Typeable t, Show t, MatchBool b) =>
      TyProxy t -> BoolProxy b -> Dict (IsTrav (ScatterTy t b))
    travOneTy Proxy b = case matchBool b of
      STrue  -> Dict
      SFalse -> Dict


travScatAPf :: TraversibleL l =>
  ProxyList l -> Dict (TraversibleL (ScatterAll l))
travScatAPf p = case matchPL p of
  EmptyL -> Dict
  NonEmpty (_,xs) -> case travScatAPf xs of
    Dict -> Dict


scatterLenPf :: forall (bs :: [Bool]) l .
  ( QEmpty bs, QEmpty l
  , Length l ~ Length bs ) =>
  ProxyList l -> BoolList bs -> Length l :~: Length (ScatterList l bs)
scatterLenPf p bs = case (matchPL p, matchPL bs) of
  (EmptyL, EmptyL) -> Refl
  (NonEmpty (_,xs), NonEmpty (_,ys)) ->
    scatterLenPf xs ys &>> Refl


scatterAllLenPf :: TraversibleL l =>
  ProxyList l -> Length l :~: Length (ScatterAll l)
scatterAllLenPf p = case matchPL p of
  EmptyL -> Refl
  NonEmpty (_,xs) -> scatterAllLenPf xs &>> Refl


-- *** Fancy types needed for the proofs

data Dict :: Constraint -> Type where
  Dict :: c => Dict c

type IsTrav t = (Typeable t, Show t)

type BoolProxy (b :: Bool) = Proxy b


-- *** Type level Bool matching

type family AllMatchable (bs :: [Bool] ) :: Constraint where
  AllMatchable '[] = ()
  AllMatchable (b:bs) = (MatchBool b, AllMatchable bs)

data SBool :: Bool -> Type where
  STrue  :: SBool 'True
  SFalse :: SBool 'False

class MatchBool (b :: Bool) where
  matchBool :: BoolProxy b  -> SBool b

instance MatchBool 'True where
  matchBool _ = STrue

instance MatchBool 'False where
  matchBool _ = SFalse











