import qualified CAS.ContentStore
import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit Tests"
  [ CAS.ContentStore.tests
  ]
