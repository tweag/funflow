{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Funflow.Flow
  ( Flow,
    RequiredStrands,
    RequiredCoreTasks,
    toFlow,
    pureFlow,
    ioFlow,
    dockerFlow,
    putDir,
    getDir,
  )
where

import Control.Arrow (Arrow, ArrowChoice)
import Control.Kernmantle.Caching (ProvidesCaching)
import Control.Kernmantle.Rope (AnyRopeWith, HasKleisli, strand)
import Control.Monad.IO.Class (MonadIO)
import Data.CAS.ContentStore as CS
import Funflow.Tasks.Docker (DockerTask (DockerTask), DockerTaskConfig, DockerTaskInput)
import Funflow.Tasks.Simple (SimpleTask (IOTask, PureTask))
import Funflow.Tasks.Store (StoreTask (GetDir, PutDir))
import Path (Abs, Dir, Path)

-- | The constraints on the set of "strands"
--   These will be "interpreted" into "core tasks" (which have contraints defined below).
type RequiredStrands =
  '[ '("simple", SimpleTask),
     '("store", StoreTask),
     '("docker", DockerTask)
   ]

-- | The class constraints on the "core task".
--   The "core task" is the task used to run any kind of "binary task" ("strand")
type RequiredCoreTasks m =
  '[ -- Basic requirement
     Arrow,
     ArrowChoice,
     -- Support IO
     HasKleisli m,
     -- Support caching
     ProvidesCaching
   ]

-- | Flow is the main type of Funflow.
--   It is a task that takes an input value of type `input` and produces an output value of type `output`.
--   It can use any named task (strand) that is defined in @RequiredStrands@.
type Flow input output =
  forall m.
  (MonadIO m) =>
  AnyRopeWith
    RequiredStrands
    (RequiredCoreTasks m)
    input
    output


-- ** Smart constructors
--
-- Directly make a flow using @IsFlow@'s @toFlow@

-- | Allows to register on which strand a binary task should be
class IsFlow binEff where
  toFlow :: binEff i o -> Flow i o

instance IsFlow SimpleTask where
  toFlow = strand #simple

instance IsFlow DockerTask where
  toFlow = strand #docker

instance IsFlow StoreTask where
  toFlow = strand #store

pureFlow :: (i -> o) -> Flow i o
pureFlow = toFlow . PureTask

ioFlow :: (i -> IO o) -> Flow i o
ioFlow = toFlow . IOTask

dockerFlow :: DockerTaskConfig -> Flow DockerTaskInput CS.Item
dockerFlow = toFlow . DockerTask

putDir :: Flow (Path Abs Dir) CS.Item
putDir = toFlow PutDir

getDir :: Flow (CS.Item) (Path Abs Dir)
getDir = toFlow GetDir