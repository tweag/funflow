{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE TypeInType     #-}
{-# LANGUAGE TypeOperators  #-}
module Data.Type.Equality ( module DTE, (&>>) ) where
import           "base" Data.Type.Equality
    ((:~:) (..), gcastWith)
import qualified "base" Data.Type.Equality as DTE
-- | A convenient infixr form of gcastWith.
(&>>) :: a :~: b -> (a ~ b => x) -> x
(&>>) = gcastWith
infixr 5 &>>
