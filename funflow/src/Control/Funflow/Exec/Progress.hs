-- | Progress reports from a running executor.
module Control.Funflow.Exec.Progress
  ( GraphUpdate(..)
  , NodeId
  , Progress(..)
  , Status(..)
  , NodeUpdate(..)
  , metadata
  , inputHash
  , outputHash
  , status
  , mkNodeId
  ) where

import           Control.Funflow.ContentHashable (ContentHash)
import           Control.Monad.IO.Class          (MonadIO, liftIO)
import qualified Data.Text                       as T
import           System.Random                   (randomIO)
import Data.ByteString (ByteString)

newtype NodeId = NodeId Int
  deriving Show

-- | Create a new node id. Allows us to easily change what we use for a node
-- identifier later.
mkNodeId :: MonadIO m => m NodeId
mkNodeId = liftIO $ NodeId <$> randomIO

data Status
  = Started
  | Cached
  | Finished
  | Errored
  deriving Show

data NodeUpdate
  = AddMetadata (T.Text, ByteString)
  | AddInputsHash ContentHash
  | AddOutputsHash ContentHash
  | UpdateStatus Status
  deriving Show

metadata :: NodeId -> (T.Text, ByteString) -> Progress
metadata nid = GraphUpdate . UpdateNode nid . AddMetadata

inputHash :: NodeId -> ContentHash -> Progress
inputHash nid = GraphUpdate . UpdateNode nid . AddInputsHash

outputHash :: NodeId -> ContentHash -> Progress
outputHash nid = GraphUpdate . UpdateNode nid . AddOutputsHash

status :: NodeId -> Status -> Progress
status nid = GraphUpdate . UpdateNode nid . UpdateStatus

data GraphUpdate
    -- | Introduce a node, with its parent nodes
  = IntroduceNode NodeId [NodeId]
  | UpdateNode NodeId NodeUpdate
  deriving Show

data Progress
  = Msg T.Text
  | GraphUpdate GraphUpdate
  deriving Show
