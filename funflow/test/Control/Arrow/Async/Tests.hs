{-# LANGUAGE GADTs #-}
module Control.Arrow.Async.Tests (tests) where
import           Control.Arrow
import           Control.Arrow.Async
import           Control.Arrow.Free
import           Control.Category
import           Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import           Prelude                 hiding (id, (.))
import           Test.Tasty
import           Test.Tasty.HUnit

data Step a b where
  Step :: (a -> IO b) -> Step a b

runStep :: Step a b -> AsyncA IO a b
runStep (Step f) = AsyncA $ \a -> f a

type Flow = Free Step

tests :: TestTree
tests = testGroup "AsyncA"
  [ testCase "parallel arrows executed in parallel" $ do
      sem <- newEmptyMVar
      let flow1 = effect . Step $ \() -> takeMVar sem
          flow2 = effect . Step $ \i -> putMVar sem i
          flow :: Flow ((), Int) (Int, ())
          flow = flow1 *** flow2
      (out1, out2) <- runAsyncA (eval runStep flow) ((), 3)
      assertEqual "Should finish" ((), 3) (out2, out1)
  ]
