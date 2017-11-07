{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Definition of external tasks
module Control.FunFlow.External where

import           Control.FunFlow.ContentHashable (ContentHash, ContentHashable)
import           Control.Lens.TH
import           Data.Store                      (Store)
import qualified Data.Text                       as T
import           GHC.Generics                    (Generic)

-- | A monomorphic description of an external task. This is basically just
--   a command which can be run.
data ExternalTask = ExternalTask {
    _etCommand       :: T.Text
  , _etParams        :: [T.Text]
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
