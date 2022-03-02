{-# OPTIONS -Wall #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
import GHC.Generics (Generic)
import Data.CAS.ContentHashable
import Control.Monad (when)

-- | This type exists to test the generic implementation of 'contentHash'.
-- It includes newtypes, product types, sum types, primitive types.
data Foo = Foo
  { ident :: Fiz Int,
    name :: String,
    dog :: Maybe Int,
    bli :: Float,
    blo :: Either Bar Int,
    blz :: Either Bar Bar
  }
  deriving (Generic, ContentHashable m)

data Bar = Bar Float Int Float
  deriving (Generic, ContentHashable m)

newtype Fiz a = Fiz a
  deriving (Generic, ContentHashable m)

main :: IO ()
main = do
  -- This should not take much memory. The toplevel 'Foo' structure is shared
  -- between the 100K instances, so with overhead, this should not take much
  -- more than a few MiB.
  -- Computing the hash should also be constant space (i.e. a hash is a small
  -- structure and it should walk lazyly in the main "huge" structure).
  rawHash <- contentHash $ replicate 100000 (Foo (Fiz 10) "hello" (Just 5) 123.12 (Left (Bar 1 1 2)) (Right (Bar 4 5 6)))

  -- This hash should not change, unless the hashing algorithm change or the order of the walk in the structure changes.
  let 
      expected = "ContentHash \"7f953cc9aaf79bac4d02e70dca91da1ee630b4a7894aeca9ed61942cc3a20d8a\""
  when (show rawHash /= expected) $ error $ "Raw hash is different than expected: " <> show rawHash
