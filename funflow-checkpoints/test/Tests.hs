{-# LANGUAGE Arrows                     #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeApplications           #-}
import           Control.Arrow
import           Control.Arrow.Free
import           Control.Category
import           Control.Exception.Safe
import           Control.Funflow
import           Control.Funflow.Checkpoints
import           Control.Monad               (guard)
import           Path.IO
import           Prelude                     hiding ((.))
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

newtype MyFlow eff ex a b = MyFlow { unMyFlow :: Flow eff ex a b }
  deriving (Arrow, Category, ArrowChoice, ArrowError ex, ArrowFlow eff ex)

checkedFlow :: (CheckpointT MyFlow NoEffect SomeException) () Int
checkedFlow = proc () -> do
  a <- checkpoint "start" -< (12345 :: Int)
  b <- step (*2) -< a
  c <- checkpoint "after_step" -< b
  d <- case c of
    24690 -> returnA -< 9
    _     -> checkpoint "in_cond" -< c
  returnA -< d

tests :: TestTree
tests = testGroup "Checkpoints"
  [ testCase "Flow runs to completion" $ do
      let unchecked = extractToCompletion checkedFlow
      res <- runMyFlow unchecked ()
      case res of
        Right x -> assertEqual "Should return 9" 9 x
        Left ex -> assertFailure $ show ex
  , testCase "Flow aborts at checkpoint" $ do
      let flow = extractToCheckpoint "start" checkedFlow
      res <- runMyFlow flow ()
      case res of
        Right x -> assertEqual "Should return 12345" (12345 :: Int) x
        Left ex -> assertFailure $ show ex
  , testCase "Flow aborts at checkpoint after step" $ do
      let flow = extractToCheckpoint "after_step" checkedFlow
      res <- runMyFlow flow ()
      case res of
        Right x -> assertEqual "Should return 24690" (24690 :: Int) x
        Left ex -> assertFailure $ show ex
  , testCase "Flow errors at checkpoint in conditional" $
      assertException "CheckpointInConditional" (CheckpointInConditional "in_cond")
        $ let !_flow = extractToCheckpoint @Int "in_cond" checkedFlow in return ()
  ]

runMyFlow :: MyFlow NoEffect SomeException a b -> a -> IO (Either SomeException b)
runMyFlow flow input = withSystemTempDir "test_output_" $ \tmpPath ->
  withSimpleLocalRunner tmpPath $ \run -> run (unMyFlow flow) input

assertException :: (Exception e, Eq e) => String -> e -> IO a -> IO ()
assertException preface expected action =
    handleJust isWanted (const $ return ()) $ do
        _ <- action
        assertFailure msg
  where
    isWanted = guard . (== expected)
    msg = preface ++ "\nexpected exception: " ++ show expected
