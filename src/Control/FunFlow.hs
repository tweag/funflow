{-# LANGUAGE Arrows, GADTs, OverloadedStrings, TupleSections #-}

module Control.FunFlow where

import Control.FunFlow.Base

import Data.Aeson
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Control.Monad.State.Strict
import Data.Monoid ((<>))
import Control.Exception

collectNames :: Flow a b -> [T.Text]
collectNames (Name n f) = n : collectNames f
collectNames (Step _) = []
collectNames (Arr _) = []
collectNames (Compose f g) = collectNames f ++ collectNames g
collectNames (First f) = collectNames f

-- | a fresh variable supply
newtype Freshers = Freshers { unFreshers :: [T.Text] }
  deriving Show

genFreshersPrefixed :: T.Text -> Freshers
genFreshersPrefixed p = Freshers $ map ((p<>) . T.pack . show) [(0::Int)..]

initFreshers :: Freshers
initFreshers = genFreshersPrefixed ""

popFreshers :: Freshers -> (T.Text, Freshers)
popFreshers (Freshers (f:fs)) = (f, Freshers fs)

