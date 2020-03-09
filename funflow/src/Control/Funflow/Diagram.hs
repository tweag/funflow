{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE RankNTypes            #-}

-- | A Diagram is a representation of an arrow with labels.
--   No computation is actually performed by a diagram, it just carries
--   around a description.
--   We explot the fact that in 7.10,
--   everything is typeable to label the incoming and outgoing node types.
module Control.Funflow.Diagram where

import           Control.Arrow
import           Control.Arrow.Free (ArrowError (..))
import           Control.Category
import qualified Data.Profunctor    as P
import qualified Data.Profunctor.Traversing as P
import           Data.Proxy         (Proxy (..))
import qualified Data.Text          as T
import           Prelude            hiding (id, (.))


newtype NodeProperties = NodeProperties {
  labels :: [T.Text]
}

emptyNodeProperties :: NodeProperties
emptyNodeProperties = NodeProperties []

data Diagram ex a b where
  Node :: NodeProperties
       -> Proxy a
       -> Proxy b
       -> Diagram ex a b
  Seq :: Diagram ex a b -> Diagram ex b c -> Diagram ex a c
  Par :: Diagram ex a b -> Diagram ex c d -> Diagram ex (a,c) (b,d)
  Fanin :: Diagram ex a c -> Diagram ex b c -> Diagram ex (Either a b) c
  Try :: Diagram ex a b -> Diagram ex a (Either ex b)
  Traverse :: Diagram ex a b -> Diagram ex (f a) (f b)
  Wander :: (forall f. (Applicative f) => (a -> f b) -> s -> f t) -> Diagram ex a b -> Diagram ex s t
  deriving (P.Profunctor, P.Strong, P.Choice) via P.WrappedArrow (Diagram ex)

instance Category (Diagram ex) where
  id = Node emptyNodeProperties Proxy Proxy
  (.) = flip Seq

instance Arrow (Diagram ex) where
  arr :: forall a b. (a -> b) -> Diagram ex a b
  arr = const $ Node emptyNodeProperties (Proxy :: Proxy a) (Proxy :: Proxy b)
  first f = Par f id
  second f = Par id f
  (***) = Par

instance ArrowChoice (Diagram ex) where
  f +++ g = (f >>> arr Left) ||| (g >>> arr Right)
  f ||| g = Fanin f g

instance ArrowError ex (Diagram ex) where
  try = Try

instance P.Traversing (Diagram ex) where
  traverse' = Traverse
  wander = Wander

-- | Construct a labelled node
node :: forall arr a b ex. Arrow arr => arr a b -> [T.Text] -> (Diagram ex) a b
node _ lbls = Node props (Proxy :: Proxy a) (Proxy :: Proxy b)
  where props = NodeProperties lbls
