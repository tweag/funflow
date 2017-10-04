{-# LANGUAGE Arrows, GADTs, OverloadedStrings, TupleSections #-}

module Control.FunFlow where

import Control.FunFlow.Base

import qualified Data.Text as T
import Data.Monoid ((<>))

collectNames :: Flow a b -> [T.Text]
collectNames (Name n f) = n : collectNames f
collectNames (Step _) = []
collectNames (Arr _) = []
collectNames (Compose f g) = collectNames f ++ collectNames g
collectNames (First f) = collectNames f
collectNames (Par f g) = collectNames f ++ collectNames g


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

