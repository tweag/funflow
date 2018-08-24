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
{-# LANGUAGE AllowAmbiguousTypes #-} -- Used with care

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-|
Module      : Wiring
Description : 'SomeFlow' combinators

This module exports combinators for 'SomeFlow's. Chiefly, it
exports a high level combinator for taking a linear preorder of SomeFlows and
sequencing this into one "concurrent" Flow.

-}


module Control.Funflow.CWL.Convert.Wiring
  ( wireLinearPhases
  , stackFlows
  , compose
  , typedCompose
  , propagateInputs
  )
where

import           Control.Arrow
import           Control.Monad
    (foldM, (>=>))


-- Internal imports
import           Control.Funflow.CWL.Types.FunflowTypes
import           Control.Funflow.CWL.Types.Schema
import           Data.HList
import           Data.ErrorMonad



-- * Top Level Interface
--------------------------------------------------------------------------------


type Phase = [SomeFlow]
type NonEmptyPhase = (SomeFlow, [SomeFlow])
type NonEmptyPhases = (NonEmptyPhase, [NonEmptyPhase]) 
type NonEmptyPreOrder = (SomeFlow, [SomeFlow])


-- | Given a linear preorder of SomeFlows for the
--   workflow at hand, we wire them into one flow,
--   preserving all outputs along the way. So,
--   the outputs of the resulting flow is the
--   union of all the outputs of each node in the
--   preorder.
--
--   We call the inner lists of SomeFlows *phases*.
--   A phase is a list of flows that could be run
--   in parallel. We call flows that could run
--   in parallel "stackable" flows. So, restated,
--   a phase is a list of stackable flows.
wireLinearPhases :: [Phase] -> ErrM SomeFlow
wireLinearPhases = (fmap compressPhases . checkAllNonEmpty) >=> composePhases
  where

    checkAllNonEmpty :: [Phase] -> ErrM NonEmptyPhases
    checkAllNonEmpty []     = throwM $ ErrMsg 
      "Impossible error. Please report this as \
      \a bug on the repo. (Empty phase list.)"
    checkAllNonEmpty (p:ps) = do
      nonEmptyFirst <- checkNonEmpty p
      nonEmptyRest  <- traverse checkNonEmpty ps
      return (nonEmptyFirst, nonEmptyRest)
      where
        checkNonEmpty :: Phase -> ErrM NonEmptyPhase
        checkNonEmpty []     = throwM $ ErrMsg $
          "Impossible error. Please report this as \
          \a bug on the repo. (Empty phase.)"
        checkNonEmpty (f:fs) = return (f,fs)


    -- | We take a list of phases and make
    --   each one a single flow for that phase.
    --   Note that each flow is still called
    --   a phase.
    compressPhases :: NonEmptyPhases -> NonEmptyPreOrder
    compressPhases (p,ps) = (stackFlows p, map stackFlows ps)


    -- | We sequence the phases from left to right with
    --   composePhase, a function that composes
    --   two flows if the first has a ouput record
    --   that is a super-record of the input record
    --   of the second, and, propagates the inputs
    --   along to the end.
    composePhases :: NonEmptyPreOrder -> ErrM SomeFlow
    composePhases (phase, phases) = foldM composePhase phase phases
      where
        composePhase :: SomeFlow -> SomeFlow -> ErrM SomeFlow
        composePhase = curry $ fmap propagateInputs . uncurry compose


-- | Takes inputs from flow and merges those with
--   the outputs from the flow so no information
--   is lost. This is analogous to carryInput:
--   carryInput :: (a -> b) -> (a -> (b,a))
propagateInputs :: SomeFlow -> SomeFlow
propagateInputs (SomeFlow flow inSch outSch) =
  case unionSchemas inSch outSch of
    SchemaUnion outSch' joiner ->
      SomeFlow (propFlow flow joiner) inSch outSch'
  where
    propFlow :: AllTraversible [l,l',k] =>
      (HList l ==> HList l') -> ((HList l, HList l') -> HList k) ->
      HList l ==> HList k
    propFlow flow' joiner = proc hlist -> do
      hlist' <- flow' -< hlist
      returnA -< joiner (hlist, hlist')




-- * Workflow Combinators
--------------------------------------------------------------------------------

-- | Stacking flows
--   We take flows that could be run in parallel
--   and make a flow that runs in parallel.
--   Note: type NonEmptyPhase = (SomeFlow, [SomeFlow])
stackFlows :: NonEmptyPhase -> SomeFlow
stackFlows (f,fs) = foldl stackTwoFlows f fs
  where

    -- Here, we use schema manipulation (see below)
    -- inside the existential SomeFlow data type.
    stackTwoFlows :: SomeFlow -> SomeFlow -> SomeFlow
    stackTwoFlows (SomeFlow f1 f1in f1out) (SomeFlow f2 f2in f2out) =
      let
        inSch   = splitSchemas f1in f2in
        outSch  = unionSchemas f1out f2out
      in case (inSch, outSch) of
        ( SchemaSplit inSch' tof1 tof2,
          SchemaUnion outSch' joiner ) ->
            let newFlow = stackedFlow f1 f2 tof1 tof2 joiner in
              SomeFlow newFlow inSch' outSch'
      where
        stackedFlow :: AllTraversible [l1, r1, l2, r2, k1, k2] =>
          (HList l1 ==> HList r1) -> (HList l2 ==> HList r2) ->
          (HList k1 -> HList l1)  -> (HList k1 -> HList l2)  ->
          ((HList r1, HList r2) -> HList k2) ->
          (HList k1 ==> HList k2)
        stackedFlow f1' f2' tof1 tof2 joiner =
          proc inlist -> do
            let l1 = tof1 inlist
            let l2 = tof2 inlist
            result <- parallelFlow -< (l1,l2)
            returnA -< joiner result
          where
            parallelFlow = f1' *** f2'


-- | Composing two flows
--   Composes two flows iff second's output record
--   is sub-record of first's input record.
--   In this case, the ouput record from the first
--   is fed into the second's input record.
--   This is done by schema reduction. See below.
compose :: SomeFlow -> SomeFlow -> ErrM SomeFlow
compose (SomeFlow f1 f1in f1out) (SomeFlow f2 f2in f2out) = do
  newFlow <- typedCompose f1 f1out f2 f2in
  return $ SomeFlow newFlow f1in f2out


-- | This is exactly like 'compose' but with all the types clearly stated.
typedCompose :: AllTraversible [a, b, c, d] =>
  HList a ==> HList b -> Schema b ->
  HList c ==> HList d -> Schema c ->
  ErrM (HList a ==> HList d)
typedCompose f1 f1outSch f2 f2inSch = do
  Reduction red <- tryReduce f1outSch f2inSch
  return $ composedFlow f1 f2 red
  where
    composedFlow :: AllTraversible [a, b, c, d] =>
      (HList a ==> HList b) -> (HList c ==> HList d) ->
      (HList b -> HList c)  -> (HList a ==> HList d)
    composedFlow f1' f2' reduction = proc a -> do
      b <- f1' -< a
      let c = reduction b
      d <- f2' -< c
      returnA -< d



