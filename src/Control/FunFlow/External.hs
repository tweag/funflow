{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
-- | Definition of external tasks
module Control.FunFlow.External where

import           Control.FunFlow.ContentHashable (ContentHash, ContentHashable)
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
  | ParamPath !ContentHash
  | ParamEnv !T.Text
  | ParamUid
  | ParamGid
  | ParamOut
  deriving Generic

instance ContentHashable ParamField
instance Store ParamField

-- | A parameter to an external task
newtype Param = Param [ParamField]
  deriving (Generic, Monoid, Semigroup)

instance IsString Param where
  fromString s = Param [ParamText (fromString s)]

instance ContentHashable Param
instance Store Param

data ConvParam f = ConvParam
  { convPath :: ContentHash -> f FilePath
  , convEnv :: T.Text -> f T.Text
  , convUid :: f CUid
  , convGid :: f CGid
  , convOut :: f FilePath
  }

paramFieldToText :: Applicative f
  => ConvParam f -> ParamField -> f T.Text
paramFieldToText _ (ParamText txt) = pure txt
paramFieldToText c (ParamPath chash) = T.pack <$> convPath c chash
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

pathParam :: ContentHash -> Param
pathParam chash = Param [ParamPath chash]

envParam :: T.Text -> Param
envParam env = Param [ParamEnv env]

uidParam :: Param
uidParam = Param [ParamUid]

gidParam :: Param
gidParam = Param [ParamGid]

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
