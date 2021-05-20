module TUtils (boundedListOf, maxSizeListOf, minSizeListOf) where

import Test.QuickCheck

boundedListOf :: (Int -> Int -> Int) -> Int -> Gen a -> Gen [a]
boundedListOf f m g = sized $ \n -> choose (0, f m n) >>= (\k -> vectorOf k g)

maxSizeListOf :: Int -> Gen a -> Gen [a]
maxSizeListOf = boundedListOf min

minSizeListOf :: Int -> Gen a -> Gen [a]
minSizeListOf = boundedListOf max
