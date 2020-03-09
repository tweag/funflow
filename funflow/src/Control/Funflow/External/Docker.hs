{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Module supporting the use of docker containers as external tasks.
--
--   In general, an external task can be any command. This module just makes it
--   easier to specify certain tasks which will run inside docker containers. It
--   handles constructing the call to docker, mounting input and output
--   directories, and specifying the docker image and version to use.
module Control.Funflow.External.Docker
  ( Config (..)
  , toExternal
  ) where

import           Control.Arrow                    (Kleisli (..), second)
import           Control.Funflow.External
import           Control.Monad.Trans.State.Strict
import           Data.CAS.ContentHashable
import           Data.Semigroup                   (Semigroup, (<>))
import qualified Data.Text                        as T
import           GHC.Generics                     (Generic)

data Bind
  -- | No inputs
  = NoInput
  -- | Single input, and the path to its mountpoint within the system.
  | SingleInput (InputPath, FilePath)
  -- | Multiple inputs.
  | MultiInput [(InputPath, FilePath)]
  deriving Generic

instance ContentHashable IO Bind

instance Semigroup Bind where
  NoInput <> x = x
  x <> NoInput = x
  (SingleInput x) <> (SingleInput y) =
    MultiInput [x, y]
  (SingleInput x) <> (MultiInput m) =
    MultiInput $ x : m
  (MultiInput m) <> (SingleInput x) =
    MultiInput $ x : m
  (MultiInput m) <> (MultiInput m') =
    MultiInput $ m <> m'

instance Monoid Bind where
  mempty = NoInput
  mappend = (<>)

data Config = Config
  { image      :: T.Text
  , optImageID :: Maybe T.Text
  , command    :: Param
  , args       :: [Param]
  , env        :: Env
  , stdout     :: OutputCapture
  } deriving Generic

data Docker = Docker
  { dImage      :: T.Text
  , dOptImageID :: Maybe T.Text
  , dInput      :: Bind
  , dCommand    :: Param
  , dArgs       :: [Param]
  , dEnv        :: Env
  , dStdout     :: OutputCapture
  } deriving Generic

instance ContentHashable IO Docker

-- | Convertion state for mapping from 'Config' to 'Docker'
data ConvertState = ConvertState
  { _csInput :: Bind
    -- | Fresh name generator
  , _csFresh ::  Int
  }

toDocker :: Config -> Docker
toDocker cfg = Docker
    { dImage = image cfg
    , dOptImageID = optImageID cfg
    , dInput = input'
    , dCommand = command'
    , dArgs = args'
    , dEnv = env'
    , dStdout = stdout cfg
    }
  where
    initState = ConvertState NoInput 0
    ((command', args', env'), ConvertState input' _) = flip runState initState $ do
      command'' <- transformParam (command cfg)
      args'' <- mapM transformParam (args cfg)
      env'' <- case env cfg of
        EnvInherit -> pure EnvInherit
        EnvExplicit x -> EnvExplicit <$> mapM (runKleisli $ second $ Kleisli transformParam) x
      return (command'', args'', env'')
    transformParam :: Param -> State ConvertState Param
    transformParam (Param pfs) = Param <$> mapM transformParamField pfs
    transformParamField :: ParamField -> State ConvertState ParamField
    transformParamField (ParamPath ip) = do
      ConvertState input fresh <- get
      put $ ConvertState
        (input <> SingleInput (ip, mkInputPath fresh))
        (fresh + 1)
      return $ ParamText (T.pack $ mkInputPath fresh)
    transformParamField po = return po
    mkInputPath :: Int -> String
    mkInputPath x = "/input/" <> show x <> "/"

toExternal :: Config -> ExternalTask
toExternal (toDocker -> cfg) = ExternalTask
  -- XXX: Allow to configure the path to the docker executable.
  { _etCommand = "docker"
  , _etParams =
      [ "run"
      , "--user=" <> uidParam
      , "--workdir=/output"
      ] ++ mounts ++
      [ imageArg
      , dCommand cfg
      ] ++ dArgs cfg
  , _etEnv = dEnv cfg
  , _etWriteToStdOut = dStdout cfg
  }
  where
    mounts = outputMount : inputMounts
    mount src dst =
      "--volume=" <> pathParam src <> ":" <> stringParam dst
    outputMount = "--volume=" <> outParam <> ":/output"
    inputMounts = case dInput cfg of
      NoInput -> []
      SingleInput (chash, dest) -> [ mount chash dest ]
      MultiInput cmap ->
        [ mount chash dest
        | (chash, dest) <- cmap
        ]
    imageArg = textParam $ case dOptImageID cfg of
      Nothing  -> dImage cfg
      Just id' -> dImage cfg <> ":" <> id'
