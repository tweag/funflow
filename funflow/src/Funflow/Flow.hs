{-# LANGUAGE FlexibleContexts #-}
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
    ExtendedFlow,
    RequiredStrands,
    RequiredCore,
    IsFlow,
    toFlow,
    pureFlow,
    ioFlow,
    dockerFlow,
    putDirFlow,
    getDirFlow,
    throwStringFlow,
    returnFlow,
  )
where

import Control.Arrow (Arrow, ArrowChoice, returnA)
import Control.Exception.Safe (SomeException, StringException, throwString)
import Control.Kernmantle.Caching (ProvidesCaching)
import Control.Kernmantle.Error (ThrowEffect, TryEffect)
import Control.Kernmantle.Rope (AnyRopeWith, HasKleisli, strand)
import Control.Monad.IO.Class (MonadIO)
import Data.CAS.ContentStore as CS
import Docker.API.Client (DockerClientError)
import Funflow.Tasks.Docker (DockerTask (DockerTask), DockerTaskConfig, DockerTaskInput)
import Funflow.Tasks.Simple (SimpleTask (IOTask, PureTask))
import Funflow.Tasks.Store (StoreTask (GetDir, PutDir))
import Path (Abs, Dir, Path)
import Funflow.Type.Family.List (type (++))

-- | The constraints on the set of "strands"
--   These will be "interpreted" into "core tasks" (which have contraints defined below).
type RequiredStrands =
  '[ '("simple", SimpleTask),
     '("store", StoreTask),
     '("docker", DockerTask)
   ]

-- | The class constraints on the "core task".
--   The "core task" is the task used to run any kind of "binary task" ("strand")
type RequiredCore m =
  '[ -- Basic requirement
     Arrow,
     ArrowChoice,
     -- Error handling
     ThrowEffect SomeException,
     TryEffect SomeException,
     ThrowEffect StringException,
     TryEffect StringException,
     ThrowEffect DockerClientError,
     TryEffect DockerClientError,
     -- Support IO
     HasKleisli m,
     -- Support caching
     ProvidesCaching
   ]

-- | Flow is the main type of Funflow.
--   It is a task that takes an input value of type `input` and produces an output value of type `output`.
--   It can use any named task (strand) that is defined in @RequiredStrands@.
type Flow input output = ExtendedFlow '[] input output

-- | Allows to add other strands on top of the existing strands used by funflow's @Flow@ defined by @RequiredStrands@.
--   Thoses additional strands should be weaved before passing the resulting loose rope to runFlow.
--   See the advanced tutorial on extending funflow's @Flow@.
type ExtendedFlow additionalStrands input output =
  forall m.
  (MonadIO m) =>
  AnyRopeWith
    (additionalStrands ++ RequiredStrands)
    (RequiredCore m)
    input
    output

-- ** Smart constructors

-- Directly make a flow using @IsFlow@'s @toFlow@

-- | Allows to register on which strand a binary task should be
class IsFlow binEff where
  toFlow :: binEff i o -> Flow i o

instance IsFlow SimpleTask where
  toFlow = strand #simple

-- | Make a flow from a pure function
pureFlow :: (i -> o) -> Flow i o
pureFlow = toFlow . PureTask

-- | Make a flow from an IO monad
ioFlow :: (i -> IO o) -> Flow i o
ioFlow = toFlow . IOTask

instance IsFlow DockerTask where
  toFlow = strand #docker

-- | Make a flow from the configuration of a Docker task
dockerFlow :: DockerTaskConfig -> Flow DockerTaskInput CS.Item
dockerFlow = toFlow . DockerTask

instance IsFlow StoreTask where
  toFlow = strand #store

-- | Make a flow to put a directory into the content store
putDirFlow :: Flow (Path Abs Dir) CS.Item
putDirFlow = toFlow PutDir

-- | Make a flow to get the absolute path of the directory storing the data of an item in the content store
getDirFlow :: Flow (CS.Item) (Path Abs Dir)
getDirFlow = toFlow GetDir

-- | Make a flow that throws an exception with a message
throwStringFlow :: Flow String ()
throwStringFlow = ioFlow $ \message -> throwString message

-- | Return a result at the end of a flow
returnFlow :: Flow a a
returnFlow = returnA