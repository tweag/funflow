{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
module Control.FunFlow.Checkpoints
  ( CheckpointException(..)
  , CheckpointT
  , checkpoint
  , extractToCompletion
  , extractToCheckpoint
  , extractToCheckpointEither
  ) where

import           Control.Arrow
import           Control.Arrow.Free
import           Control.Category
import           Control.Exception.Base (Exception, throw)
import           Control.FunFlow
import           Data.Either            (Either (..))
import           Data.Eq                (Eq, (==))
import           Data.Function          (flip, ($))
import           Data.Maybe             (Maybe (..))
import           Data.Proxy
import           Data.String            (String)
import           Data.Tuple             (fst, snd)
import           Data.Typeable
import           Text.Show              (Show)

data CheckpointException =
    -- | A checkpoint has been defined multiple times in the flow
    RepeatedCheckpoint String
    -- | A conditional branch has a checkpoint in.
  | CheckpointInConditional String
    -- | A branch responsible for catching errors has a checkpoint in.
  | CheckpointInCatch String
    -- | No matching checkpoint has been defined in the flow
  | NoCheckpoint String
  deriving (Eq, Show)

instance Exception CheckpointException

data CheckpointRef a = CheckpointRef (Proxy a) String
  deriving Eq

-- | Get the name of a constructed checkpoint
checkpointName :: CheckpointRef a -> String
checkpointName (CheckpointRef _ x) = x

-- | A checkpoint arrow transfomer.
data CheckpointT arr (eff :: * -> * -> *) ex a b where
  Checkpoint :: Typeable a => CheckpointRef a -> CheckpointT arr eff ex a a
  Unchecked :: arr eff ex a b -> CheckpointT arr eff ex a b
  Seq :: CheckpointT arr eff ex a b -> CheckpointT arr eff ex b c -> CheckpointT arr eff ex a c
  Par :: CheckpointT arr eff ex a b -> CheckpointT arr eff ex c d -> CheckpointT arr eff ex (a,c) (b,d)
  Fanin :: ArrowChoice (arr eff ex) => CheckpointT arr eff ex a c -> CheckpointT arr eff ex b c -> CheckpointT arr eff ex (Either a b) c
  Catch :: ArrowError ex (arr eff ex) => CheckpointT arr eff ex a b -> CheckpointT arr eff ex (a,ex) b -> CheckpointT arr eff ex a b

instance Category (arr eff ex) => Category (CheckpointT arr eff ex) where
  id = Unchecked id
  (.) = flip Seq
instance Arrow (arr eff ex) => Arrow (CheckpointT arr eff ex) where
  arr f = Unchecked $ arr f
  (***) = Par
instance ArrowChoice (arr eff ex) => ArrowChoice (CheckpointT arr eff ex ) where
  f +++ g = (f >>> arr Left) ||| (g >>> arr Right)
  f ||| g = Fanin f g
instance ArrowError ex (arr eff ex) => ArrowError ex (CheckpointT arr eff ex) where
  f `catch` g = Catch f g

instance ArrowFlow eff ex (arr eff ex) => ArrowFlow eff ex (CheckpointT arr eff ex) where
  step' props = Unchecked . step' props
  stepIO' props = Unchecked . stepIO' props
  external = Unchecked . external
  external' p eff = Unchecked $ external' p eff
  wrap' p eff = Unchecked $ wrap' p eff
  putInStore = Unchecked . putInStore
  getFromStore = Unchecked . getFromStore
  internalManipulateStore = Unchecked . internalManipulateStore

-- | Construct a checkpoint in a flow.
checkpoint :: Typeable a => String -> CheckpointT arr eff ex a a
checkpoint = Checkpoint . CheckpointRef (Proxy :: Proxy a)

-- | Run a checkpointed flow to completion
extractToCompletion :: Arrow (arr eff ex)
                    => CheckpointT arr eff ex a b -> arr eff ex a b
extractToCompletion (Checkpoint _) = id
extractToCompletion (Unchecked x)  = x
extractToCompletion (Seq a b)      = extractToCompletion b . extractToCompletion a
extractToCompletion (Par a b)      = extractToCompletion a *** extractToCompletion b
extractToCompletion (Fanin a b)    = extractToCompletion a ||| extractToCompletion b
extractToCompletion (Catch a b)    = extractToCompletion a `catch` extractToCompletion b

-- | Run a flow to a checkpoint.
--
--   This will return either
--   - 'Left flow' with the original flow if no checkpoint was reached.
--   - 'Right flow' which runs to a checkpoint.
--
--   It may error if the flow has two checkpoints with the same name (and type),
--   but this is not guaranteed. Please don't do that.
--
--   It will error if a checkpoint is set down a conditional branch.
extractToCheckpointEither :: forall c arr eff ex a b.
                        ( Arrow (arr eff ex)
                        , Typeable c
                        )
                      => String
                      -> CheckpointT arr eff ex a b
                      -> Either (arr eff ex a b) (arr eff ex a c)
extractToCheckpointEither chkName = go where
  ref = CheckpointRef (Proxy :: Proxy c) chkName
  go :: forall i o. CheckpointT arr eff ex i o
     -> Either (arr eff ex i o) (arr eff ex i c)
  go (Checkpoint ref') = case eqT :: (Maybe (c :~: i)) of
    -- In this case, we have a checkpoint but it's the wrong type. So replace it with a no-op arrow
    Nothing -> Left id
    -- Otherwise, we have found a checkpoint of the right type.
    Just Refl -> if ref' == ref
      -- We have hit the checkpoint. Return the value on the right and mark this branch
      then Right id
      -- We are of the correct type but wrong reference
      else Left id
  -- For an unlifted effect, just lift its result into the either
  go (Unchecked unlifted) = Left unlifted
  -- We have sequential composition. We want to check first whether the first action hits a checkpoint. If
  -- it does, we throw away the following computation and short-circuit on the 'Right' branch.
  go (Seq doThis thenThat) = case go doThis of
    -- The first arrow has hit a checkpoint. Discard the follow-up and return it
    Right doneThis -> Right doneThis
    -- No checkpoint in the first arrow. Run the second arrow
    Left doneThis -> case go thenThat of
      -- Second arrow hit a checkpoint. Compose in 'Right'
      Right doneThat -> Right $ doneThat . doneThis
      -- No checkpoint in the second arrow. Compose in 'Left'
      Left doneThat  -> Left $ doneThat . doneThis
  -- Parallel execution. If either branch has a checkpoint in, we will discard the other branch and
  -- return the result from the checkpoint. If both branches have a checkpoint in, we will error
  go (Par doLeft doRight) = case (go doLeft, go doRight) of
    -- No checkpoints on either side. Great
    (Left doneLeft, Left doneRight) -> Left $ doneLeft *** doneRight
    -- Checkpoint on the left. We discard whatever happens on the right
    (Right doneLeft, Left _) -> Right $ first doneLeft >>> arr fst
    -- Checkpoint on the right. We discard whatever happens on the left
    (Left _, Right doneRight) -> Right $ second doneRight >>> arr snd
    -- Checkpoint on both sides. Somebody is playing silly buggers
    _ -> throw $ RepeatedCheckpoint (checkpointName ref)
  -- Either do the left or the right. The issue here is that, even if we find
  -- a checkpoint, we don't know don't know whether it will be hit. We get around this by
  -- simply forbidding a checkpoint in a conditional branch.
  go (Fanin doLeft doRight) = case (go doLeft, go doRight) of
    -- No checkpoints on either side. Great
    (Left doneLeft, Left doneRight) -> Left $ doneLeft ||| doneRight
    -- Checkpoint in a conditional. Throw an error.
    _ -> throw $ CheckpointInConditional (checkpointName ref)
  -- Catch branch. Firstly, we check whether there are any checkpoints in the error handling case.
  -- If there are, we throw an error, because what does that even mean? If there aren't, then
  -- we can carry out the `doThis` branch, but we lose error handling if it has a checkpoint.
  go (Catch doThis orElseThat) = case go orElseThat of
    Right _ -> throw $ CheckpointInCatch (checkpointName ref)
    Left doneElseThat -> case go doThis of
      Left doneThis  -> Left $ doneThis `catch` doneElseThat
      Right doneThis -> Right doneThis

-- | Run a flow to a checkpoint.
--
--   This will error if no matching checkpoint is found in the flow,
--   or if that checkpoint is defined in an invalid place.
extractToCheckpoint :: forall c arr eff ex a b.
                  ( Arrow (arr eff ex)
                  , Typeable c
                  )
                => String
                -> CheckpointT arr eff ex a b
                -> arr eff ex a c
extractToCheckpoint cpName flow = case extractToCheckpointEither cpName flow of
  Left _  -> throw $ NoCheckpoint cpName
  Right f -> f
