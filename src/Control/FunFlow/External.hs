{-# LANGUAGE DeriveGeneric #-}
-- | Definition of external tasks
module Control.FunFlow.External where

import           Control.FunFlow.ContentHashable (ContentHash)
import           Data.Store   (Store)
import qualified Data.Text    as T
import           GHC.Generics (Generic)

-- | A monomorphic description of an external task. This is basically just
--   a command which can be run.
data ExternalTask = ExternalTask {
    _etCommand :: T.Text
  , _etParams  :: [T.Text]
} deriving Generic

instance Store ExternalTask

data TaskDescription = TaskDescription {
    _tdOutput :: ContentHash
  , _tdTask   :: ExternalTask
  } deriving Generic


