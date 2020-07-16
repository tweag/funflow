module Funflow.Util where

import Data.Function (on)

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f = uncurry ((,) `on` f)
