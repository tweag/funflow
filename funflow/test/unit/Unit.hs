-- Funflow unit tests

import Test.Tasty
import Test.Tasty.HUnit

unitTests :: TestTree
unitTests =
  testGroup
    "unit"
    [ testCase "skeleton" $
        "foo" @?= "bar"
    ]

tests :: TestTree
tests = testGroup "Tests" [unitTests]

main :: IO ()
main = defaultMain tests
