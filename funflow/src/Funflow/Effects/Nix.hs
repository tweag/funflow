{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-
 - "Nix" effects allow to run tasks in Docker
 -}
module Funflow.Effects.Nix
  ( NixEffectConfig (..),
    NixEffect (..),
    NixpkgsSource (..),
    Environment (..),
  )
where

import Data.CAS.ContentHashable
  ( ContentHashable,
    contentHashUpdate,
    contentHashUpdate_fingerprint,
  )
import Data.Text (Text)
import GHC.Generics (Generic)
import Text.URI (URI)
import qualified Text.URI as URI

data NixpkgsSource
  = -- | Inherit the @NIX_PATH@ from the environment
    NIX_PATH
  | -- | The 'URI' pointing to a nixpkgs tarball
    NixpkgsTarball URI
  deriving (Generic)

-- Configure what task to run in Docker
data NixEffectConfig = NixEffectConfig
  { -- | Specification of the nix environment
    nixEnv :: Environment,
    -- | Which version of nixpkgs to use
    nixpkgsSource :: NixpkgsSource,
    -- | The command to run in the environment
    command :: Text,
    -- | Arguments to pass to the command
    args :: [Text],
    -- | Environmental variables which are set in the environment
    env :: [(Text, Text)]
  }

data Environment
  = -- | Path to a shell.nix file
    ShellFile (Text)
  | -- | A list of packages that
    -- will be passed by @-p@.
    PackageList [Text]
  deriving (Generic)

-- instances for cashing
instance ContentHashable IO Environment

instance ContentHashable IO NixpkgsSource where
  contentHashUpdate ctx NIX_PATH = contentHashUpdate_fingerprint ctx NIX_PATH
  contentHashUpdate ctx (NixpkgsTarball s) = contentHashUpdate ctx (URI.render s)

-- Docker effects to perform external tasks
data NixEffect i o where
  NixEffect :: NixEffectConfig -> NixEffect () ()
