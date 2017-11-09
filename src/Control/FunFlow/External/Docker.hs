{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Control.FunFlow.External.Docker where

import           Control.FunFlow.ContentHashable
import           Control.FunFlow.External
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as Map
import           Data.Semigroup                  ((<>))
import qualified Data.Text                       as T
import           GHC.Generics                    (Generic)
import           System.FilePath

data Bind
  -- | Single input, will get mounted to @/input@ on the image.
  = SingleInput ContentHash
  -- | Multiple inputs, each gets mouted into a subdirectory under
  -- @/input@ as described by the given mapping.
  | MultiInput (Map FilePath ContentHash)
  deriving Generic

instance ContentHashable Bind

data Config = Config
  { image :: T.Text
  , input :: Bind
  , command :: FilePath
  , args :: [T.Text]
  } deriving Generic

instance ContentHashable Config

toExternal :: Config -> ExternalTask
toExternal cfg = ExternalTask
  -- XXX: Allow to configure the path to the docker executable.
  { _etCommand = "docker"
  , _etParams =
      [ "run"
      , "--user=" <> uidParam
      ] ++ mounts ++
      [ textParam (image cfg)
      , stringParam (command cfg)
      ] ++ map textParam (args cfg)
  , _etWriteToStdOut = False
  }
  where
    mounts = outputMount : inputMounts
    mount src dst =
      "--volume=" <> pathParam src <> ":" <> stringParam dst
    outputMount = "--volume=" <> outParam <> ":/output"
    inputMounts = case input cfg of
      SingleInput chash -> [ mount chash "/input" ]
      MultiInput cmap ->
        [ mount chash ("/input" </> dest)
        | (dest, chash) <- Map.toList cmap
        ]
