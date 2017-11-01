{-# LANGUAGE GADTs               #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | A Diagram is a representation of an arrow with labels.
--   No computation is actually performed by a diagram, it just carries
--   around a description.
--   We explot the fact that in 7.10,
--   everything is typeable to label the incoming and outgoing node types.
module Control.FunFlow.Diagram where

import           Control.Arrow
import           Control.Category
import           Data.Proxy       (Proxy (..))
import qualified Data.Text        as T
import           Prelude          hiding (id, (.))

newtype NodeProperties = NodeProperties {
  labels :: [T.Text]
}

emptyNodeProperties :: NodeProperties
emptyNodeProperties = NodeProperties []

data Diagram a b where
  Node :: NodeProperties
       -> Proxy a
       -> Proxy b
       -> Diagram a b
  Seq :: Diagram a b -> Diagram b c -> Diagram a c
  Par :: Diagram a b -> Diagram c d -> Diagram (a,c) (b,d)
  Fanin :: Diagram a c -> Diagram b c -> Diagram (Either a b) c

instance Category Diagram where
  id = Node emptyNodeProperties Proxy Proxy
  (.) = flip Seq

instance Arrow Diagram where
  arr :: forall a b. (a -> b) -> Diagram a b
  arr = const $ Node emptyNodeProperties (Proxy :: Proxy a) (Proxy :: Proxy b)
  first f = Par f id
  second f = Par id f
  (***) = Par

instance ArrowChoice Diagram where
  f +++ g = (f >>> arr Left) ||| (g >>> arr Right)
  f ||| g = Fanin f g

-- | Construct a labelled node
node :: forall a b. (a -> b) -> [T.Text] -> Diagram a b
node _ labels = Node props (Proxy :: Proxy a) (Proxy :: Proxy b)
  where props = NodeProperties labels
