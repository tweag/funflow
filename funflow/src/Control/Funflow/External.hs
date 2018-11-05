{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
-- | Definition of external tasks
module Control.Funflow.External where

import           Control.Funflow.ContentHashable (ContentHash, ContentHashable, ExternallyAssuredDirectory (..),
                                                  ExternallyAssuredFile (..))
import qualified Control.Funflow.ContentStore    as CS
import           Control.Lens.TH
import           Data.Aeson                      (FromJSON, ToJSON)
#if __GLASGOW_HASKELL__ < 804
import           Data.Semigroup
#endif
import           Data.Store                      (Store)
import           Data.String                     (IsString (..))
import qualified Data.Text                       as T
import           GHC.Generics                    (Generic)
import           Path
import           System.Posix.Types              (CGid, CUid)

-- | Set of items which may be treated as an input path to an external task.
data InputPath
    -- | An item in the content store.
  = IPItem CS.Item
    -- | An external file whose contents are considered assured by the external
    -- system.
  | IPExternalFile ExternallyAssuredFile
    -- | An external directory whose contents are considered assured by the
    -- external system.
  | IPExternalDir ExternallyAssuredDirectory
  deriving (Generic, Show)

instance ContentHashable IO InputPath
instance FromJSON InputPath
instance ToJSON InputPath
instance Store InputPath

-- | Component of a parameter
data ParamField
  = ParamText !T.Text
    -- ^ Text component.
  | ParamPath !InputPath
    -- ^ Reference to a path to a content store item.
  | ParamEnv !T.Text
    -- ^ Reference to an environment variable.
  | ParamUid
    -- ^ Reference to the effective user ID of the executor.
  | ParamGid
    -- ^ Reference to the effective group ID of the executor.
  | ParamOut
    -- ^ Reference to the output path in the content store.
  | ParamCmd Param
    -- ^ A quoted command that we can pass to another program as an
    -- argument.
  deriving (Generic, Show)

instance ContentHashable IO ParamField
instance FromJSON ParamField
instance ToJSON ParamField
instance Store ParamField

-- | A parameter to an external task
--
-- The runtime values to external references, e.g. environment variables,
-- should not significantly influence the result of the external task.
-- In particular, the content hash will not depend on these runtime values.
newtype Param = Param [ParamField]
  deriving (Generic, Monoid, Semigroup, Show)

instance IsString Param where
  fromString s = Param [ParamText (fromString s)]

instance ContentHashable IO Param
instance FromJSON Param
instance ToJSON Param
instance Store Param

-- | Converter of path components.
data ConvParam f = ConvParam
  { convPath :: CS.Item -> f (Path Abs Dir)
    -- ^ Resolve a reference to a content store item.
  , convEnv  :: T.Text -> f T.Text
    -- ^ Resolve an environment variable.
  , convUid  :: f CUid
    -- ^ Resolve the effective user ID.
  , convGid  :: f CGid
    -- ^ Resolve the effective group ID.
  , convOut  :: f (Path Abs Dir)
    -- ^ Resolve the output path in the content store.
  }

paramFieldToText :: Applicative f
  => ConvParam f -> ParamField -> f T.Text
paramFieldToText _ (ParamText txt)  = pure txt
paramFieldToText c (ParamPath (IPItem item)) = T.pack . fromAbsDir <$> convPath c item
paramFieldToText _ (ParamPath (IPExternalFile (ExternallyAssuredFile item)))
  = pure . T.pack . fromAbsFile $ item
paramFieldToText _ (ParamPath (IPExternalDir (ExternallyAssuredDirectory item)))
  = pure . T.pack . fromAbsDir $ item
paramFieldToText c (ParamEnv env)   = convEnv c env
paramFieldToText c ParamUid         = T.pack . show <$> convUid c
paramFieldToText c ParamGid         = T.pack . show <$> convGid c
paramFieldToText c ParamOut         = T.pack . fromAbsDir <$> convOut c
paramFieldToText c (ParamCmd cmd)   = paramToText c cmd

-- | Transform a parameter to text using the given converter.
paramToText :: Applicative f
  => ConvParam f -> Param -> f T.Text
paramToText c (Param ps) = mconcat <$> traverse (paramFieldToText c) ps

stringParam :: String -> Param
stringParam str = Param [ParamText (T.pack str)]

textParam :: T.Text -> Param
textParam txt = Param [ParamText txt]

-- | Reference to a path to either:
--   - a content store item, or
--   - an externally assured file/directory.
pathParam :: InputPath -> Param
pathParam item = Param [ParamPath item]

-- | Reference to a path to a file or directory within a store item.
contentParam :: CS.Content t -> Param
contentParam (CS.All item) = pathParam $ IPItem item
contentParam (item CS.:</> path) =
  pathParam (IPItem item) <> stringParam (toFilePath path)

-- | Reference an externally assured file
externalFileParam :: ExternallyAssuredFile -> Param
externalFileParam = pathParam . IPExternalFile

-- | Reference an externally assured file
externalDirectoryParam :: ExternallyAssuredDirectory -> Param
externalDirectoryParam = pathParam . IPExternalDir

-- | Reference to an environment variable.
envParam :: T.Text -> Param
envParam env = Param [ParamEnv env]

-- | Reference to the effective user ID of the executor.
uidParam :: Param
uidParam = Param [ParamUid]

-- | Reference to the effective group ID of the executor.
gidParam :: Param
gidParam = Param [ParamGid]

-- | Reference to the output path in the content store.
outParam :: Param
outParam = Param [ParamOut]

-- | Control how and where stdout from the process is captured. Some external
-- steps will write their output to stdout rather than to a file.
data OutputCapture
    -- | Specify that the step will write its output files directly, and that
    --   stdout will not be captured in the step output.
  = NoOutputCapture
    -- | Capture output to a file named 'out' in the output directory.
  | StdOutCapture
    -- | Capture output to a custom named file in the output directory.
  | CustomOutCapture (Path Rel File)
  deriving (Generic, Show)

-- | Get the file to write output to, if this is desired.
outputCaptureToRelFile :: OutputCapture -> Maybe (Path Rel File)
outputCaptureToRelFile NoOutputCapture         = Nothing
outputCaptureToRelFile StdOutCapture           = Just [relfile|out|]
outputCaptureToRelFile (CustomOutCapture file) = Just file

instance ContentHashable IO OutputCapture
instance FromJSON OutputCapture
instance ToJSON OutputCapture
instance Store OutputCapture

-- | Control the environment set for the external process. This can either
--   inherit from the surrounding environment, or explicitly set things.
data Env
    -- | Inherit all environment variables from the surrounding shell. Note that
    -- the values of these variables will not be taken into account in the
    -- content hash, and so changes to them will not trigger a rerun of the
    -- step.
  = EnvInherit
  | EnvExplicit [(T.Text, Param)]
  deriving (Generic, Show)

instance ContentHashable IO Env
instance FromJSON Env
instance ToJSON Env
instance Store Env

-- | A monomorphic description of an external task. This is basically just
--   a command which can be run.
data ExternalTask = ExternalTask {
    _etCommand       :: T.Text
  , _etParams        :: [Param]
    -- ^ Environment variables to set for the scope of the execution.
  , _etEnv           :: Env
  , _etWriteToStdOut :: OutputCapture
} deriving (Generic, Show)

instance ContentHashable IO ExternalTask
instance FromJSON ExternalTask
instance ToJSON ExternalTask
instance Store ExternalTask

data TaskDescription = TaskDescription {
    _tdOutput :: ContentHash
  , _tdTask   :: ExternalTask
  } deriving (Generic, Show)

makeLenses ''ExternalTask
makeLenses ''TaskDescription
