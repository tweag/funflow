{-# LANGUAGE Arrows              #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Control.FunFlow where

import           Control.FunFlow.Base
import           Control.FunFlow.Diagram

import           Data.Monoid             ((<>))
import qualified Data.Text               as T

collectNames :: Flow a b -> [T.Text]
collectNames flow = collectNames' $ toDiagram flow
  where
    collectNames' :: forall a1 b1. Diagram a1 b1 -> [T.Text]
    collectNames' (Node (NodeProperties lbls) _ _) = lbls
    collectNames' (Seq a b) = collectNames' a ++ collectNames' b
    collectNames' (Par a b) = collectNames' a ++ collectNames' b
    collectNames' (Fanin a b) = collectNames' a ++ collectNames' b


-- | a fresh variable supply
newtype Freshers = Freshers { unFreshers :: [T.Text] }
  deriving Show

genFreshersPrefixed :: T.Text -> Freshers
genFreshersPrefixed p = Freshers $ map ((p<>) . T.pack . show) [(0::Int)..]

initFreshers :: Freshers
initFreshers = genFreshersPrefixed ""

popFreshers :: Freshers -> (T.Text, Freshers)
popFreshers (Freshers (f:fs)) = (f, Freshers fs)
popFreshers _ = error "popFreshers ran out of names! please report this a bug"


