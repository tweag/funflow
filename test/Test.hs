import qualified Control.Arrow.Async.Tests
import qualified Funflow.ContentStore
import qualified Funflow.SQLiteCoordinator
import qualified Funflow.TestFlows
import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit Tests"
  [ Funflow.ContentStore.tests
  , Control.Arrow.Async.Tests.tests
  , Funflow.TestFlows.tests
  , Funflow.SQLiteCoordinator.tests
  ]
