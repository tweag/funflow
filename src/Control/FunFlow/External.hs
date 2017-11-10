{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
-- | Definition of external tasks
module Control.FunFlow.External where

import           Control.FunFlow.ContentHashable (ContentHash, ContentHashable)
import qualified Control.FunFlow.ContentStore    as CS
import           Control.Lens.TH
import           Data.Semigroup
import           Data.Store                      (Store)
import           Data.String                     (IsString (..))
import qualified Data.Text                       as T
import           GHC.Generics                    (Generic)
import           System.Posix.Types              (CGid, CUid)

-- | Component of a parameter
data ParamField
  = ParamText !T.Text
    -- ^ Text component.
  | ParamPath !CS.Item
    -- | Reference to a path to a content store item.
  | ParamEnv !T.Text
    -- ^ Reference to an environment variable.
  | ParamUid
    -- ^ Reference to the effective user ID of the executor.
  | ParamGid
    -- ^ Reference to the effective group ID of the executor.
  | ParamOut
    -- ^ Reference to the output path in the content store.
  deriving Generic

instance ContentHashable ParamField
instance Store ParamField

-- | A parameter to an external task
--
-- The runtime values to external references, e.g. environment variables,
-- should not significantly influence the result of the external task.
-- In particular, the content hash will not depend on these runtime values.
newtype Param = Param [ParamField]
  deriving (Generic, Monoid, Semigroup)

instance IsString Param where
  fromString s = Param [ParamText (fromString s)]

instance ContentHashable Param
instance Store Param

-- | Converter of path components.
data ConvParam f = ConvParam
  { convPath :: CS.Item -> f FilePath
    -- ^ Resolve a reference to a content store item.
  , convEnv :: T.Text -> f T.Text
    -- ^ Resolve an environment variable.
  , convUid :: f CUid
    -- ^ Resolve the effective user ID.
  , convGid :: f CGid
    -- ^ Resolve the effective group ID.
  , convOut :: f FilePath
    -- ^ Resolve the output path in the content store.
  }

paramFieldToText :: Applicative f
  => ConvParam f -> ParamField -> f T.Text
paramFieldToText _ (ParamText txt) = pure txt
paramFieldToText c (ParamPath item) = T.pack <$> convPath c item
paramFieldToText c (ParamEnv env) = convEnv c env
paramFieldToText c ParamUid = T.pack . show <$> convUid c
paramFieldToText c ParamGid = T.pack . show <$> convGid c
paramFieldToText c ParamOut = T.pack <$> convOut c

-- | Transform a parameter to text using the given converter.
paramToText :: Applicative f
  => ConvParam f -> Param -> f T.Text
paramToText c (Param ps) = mconcat <$> traverse (paramFieldToText c) ps

stringParam :: String -> Param
stringParam str = Param [ParamText (T.pack str)]

textParam :: T.Text -> Param
textParam txt = Param [ParamText txt]

-- | Reference to a path to a content store item.
pathParam :: CS.Item -> Param
pathParam item = Param [ParamPath item]

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

-- | A monomorphic description of an external task. This is basically just
--   a command which can be run.
data ExternalTask = ExternalTask {
    _etCommand       :: T.Text
  , _etParams        :: [Param]
    -- | If this is set, then the process outputs on its stdout stream
    --   rather than writing to a file. In this case, output will be
    --   redirected into a file called 'out' in the output directory.
    --   Otherwise, the task is assumed to write itself to files in its
    --   working directory.
  , _etWriteToStdOut :: Bool
} deriving Generic

instance ContentHashable ExternalTask
instance Store ExternalTask

data TaskDescription = TaskDescription {
    _tdOutput :: ContentHash
  , _tdTask   :: ExternalTask
  } deriving Generic

makeLenses ''ExternalTask
makeLenses ''TaskDescription
