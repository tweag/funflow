import qualified Control.Arrow.Async.Tests
import qualified FunFlow.ContentStore
import qualified FunFlow.SQLiteCoordinator
import qualified FunFlow.TestFlows
import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit Tests"
  [ FunFlow.ContentStore.tests
  , Control.Arrow.Async.Tests.tests
  , FunFlow.TestFlows.tests
  , FunFlow.SQLiteCoordinator.tests
  ]
