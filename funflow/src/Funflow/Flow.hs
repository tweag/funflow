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
    toFlow,
    pureFlow,
    ioFlow,
    commandFlow,
    shellFlow,
    dockerFlow,
    nixFlow,
  )
where

import Control.Arrow (Arrow, ArrowChoice)
import Control.Kernmantle.Caching (ProvidesCaching)
import Control.Kernmantle.Rope (AnyRopeWith, HasKleisli, strand)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Funflow.Effects.Command (CommandEffect (..), CommandEffectConfig)
import Funflow.Effects.Docker (DockerEffect (..), DockerEffectConfig)
import Funflow.Effects.Nix (NixEffect (..), NixEffectConfig)
import Funflow.Effects.Simple (SimpleEffect (..))

-- The constraints on the set of "strands"
-- These will be "interpreted" into "core effects" (which have contraints defined below).
type RequiredStrands =
  '[  '("simple", SimpleEffect),
      '("command", CommandEffect),
      '("docker", DockerEffect),
      '("nix", NixEffect)
   ]

-- The class constraints on the "core effect".
-- The "core effect" is the effect used to run any kind of "binary effect" ("strand")
type RequiredCoreEffects m =
  '[ -- Basic requirement
     Arrow,
     ArrowChoice,
     -- Support IO
     HasKleisli m,
     -- Support caching
     ProvidesCaching
   ]

-- Flow is the main type of Funflow.
-- It is a task that takes an input of type `input` and produces an output of type `output`.
-- It can use any "user effect" ("strand") that is defined in the required strands above.
type Flow input output =
  forall m.
  (MonadIO m) =>
  AnyRopeWith
    RequiredStrands
    (RequiredCoreEffects m)
    input
    output

class IsFlow binEff where
  toFlow :: binEff i o -> Flow i o

instance IsFlow SimpleEffect where
  toFlow = strand #simple

pureFlow :: (i -> o) -> Flow i o
pureFlow = toFlow . PureEffect

ioFlow :: (i -> IO o) -> Flow i o
ioFlow = toFlow . IOEffect

instance IsFlow CommandEffect where
  toFlow = strand #command

commandFlow :: CommandEffectConfig -> Flow () ()
commandFlow = toFlow . CommandEffect

shellFlow :: Text -> Flow () ()
shellFlow = toFlow . ShellCommandEffect

instance IsFlow DockerEffect where
  toFlow = strand #docker

dockerFlow :: DockerEffectConfig -> Flow () ()
dockerFlow = toFlow . DockerEffect

instance IsFlow NixEffect where
  toFlow = strand #nix

nixFlow :: NixEffectConfig -> Flow () ()
nixFlow = toFlow . NixEffect
