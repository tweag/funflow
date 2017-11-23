{-# LANGUAGE Arrows                     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}


import           Control.Arrow
import           Control.Arrow.Promise
import           Control.Concurrent
import           Control.Exception
import           Control.FunFlow
import           Control.FunFlow.ContentHashable
import qualified Control.FunFlow.ContentStore as CS
import           Control.FunFlow.Exec.Promise
import           Control.FunFlow.Steps
import           Control.Monad
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.IO as T


slowEcho :: SimpleFlow (Int, T.Text) CS.Item
slowEcho = external $ \(n, s) -> ExternalTask
  { _etCommand = "sh"
  , _etWriteToStdOut = True
  , _etParams =
      [ "-c"
      , "sleep " <> textParam (T.pack $ show n) <> "; "
        <> "echo " <> textParam s
      ]
  }

slowCat :: SimpleFlow (Int, [CS.Item]) CS.Item
slowCat = external $ \(n, xs) -> ExternalTask
  { _etCommand = "sh"
  , _etWriteToStdOut = True
  , _etParams =
      [ "-c"
      , "sleep " <> textParam (T.pack $ show n) <> "; "
        <> "cat " <> mconcat (map (<> "/out ") $ map pathParam xs)
      ]
  }

exampleFlow :: SimpleFlow () CS.Item
exampleFlow = proc () -> do
  hello <- slowEcho -< (1, "Hello")
  world <- slowEcho -< (2, "World")
  hello_world <- slowCat -< (1, [hello, world])
  returnA -< hello_world

runExampleFlow :: IO ()
runExampleFlow = do
  let storeDir = "store"
  withSimpleLocalRunner storeDir $ \run ->
    run exampleFlow () >>= \case
      Left err -> do
        T.putStrLn $ T.replicate 50 "="
        putStrLn $ "Failed: " ++ show err
        T.putStrLn $ T.replicate 50 "="
      Right p -> runPromise p

main :: IO ()
main = do
  runExampleFlow
  promises


seconds :: Int -> Int
seconds = (* 1000000)

slowVal :: Show a => PromiseA T.Text IO (Int, a) a
slowVal = kleisli "slow value" $ \(n, a) -> PromiseT $
  promise ("slow value " <> T.pack (show a)) $ do
    T.putStrLn $ "<producing " <> T.pack (show a) <> ">"
    threadDelay (seconds n)
    return $ Ready a

-- | Independent tasks run sequentially,
-- even though they should run in parallel.
examplePromiseA1 :: PromiseA T.Text IO () T.Text
examplePromiseA1 = proc () -> do
  a <- slowVal -< (2, "Hello")
  b <- slowVal -< (1, "World")
  slowVal -< (1, a <> " " <> b)

-- | Independent tasks run in parallel
examplePromiseA2 :: PromiseA T.Text IO () T.Text
examplePromiseA2 = proc () -> do
  a <- (PromiseA $ \_ -> PromiseT $
    promise "slow hello" $ do
      T.putStrLn $ "<producing Hello>"
      threadDelay (seconds 2)
      return $ Ready "Hello")
    -< ()
  b <- (PromiseA $ \_ -> PromiseT $
    promise "slow world" $ do
      T.putStrLn $ "<producing World>"
      threadDelay (seconds 1)
      return $ Ready "World")
    -< ()
  slowVal -< (1, a <> " " <> b)

examplePromiseA3 :: PromiseA T.Text IO () T.Text
examplePromiseA3 = proc () -> do
  a <- (PromiseA $ \(Ready (n, v)) -> PromiseT $
    promise ("slow " <> v) $ do
      threadDelay (seconds n)
      return $ Ready v)
    -< (2, "Hello")
  -- *** Exception: local_examples/PromiseFlow.hs:(104,20)-(107,14): Non-exhaustive patterns in lambda
  b <- (PromiseA $ \(Ready (n, v)) -> PromiseT $
    promise ("slow " <> v) $ do
      threadDelay (seconds n)
      return $ Ready v)
    -< (1, "World")
  slowVal -< (1, a <> " " <> b)

runExamplePromise :: Show b => PromiseA T.Text IO a b -> a -> IO ()
runExamplePromise f a = do
  p <- unPromiseT $ unPromiseA f (Ready a)
  runPromise p

promises :: IO ()
promises = do
  runExamplePromise examplePromiseA1 ()
  runExamplePromise examplePromiseA2 ()
  --runExamplePromise examplePromiseA3 ()


runPromise :: Show a => Promise T.Text a -> IO ()
runPromise p = do
  r <- flip iterPromise p $ \info -> do
    T.putStrLn $ T.replicate 50 "="
    print info
    T.putStrLn $ T.replicate 50 "="
  case r of
    Left errs -> do
      T.putStrLn $ T.replicate 50 "="
      T.putStrLn "Failed:"
      mapM_ (putStrLn . displayException) errs
      T.putStrLn $ T.replicate 50 "="
    Right a   -> do
      T.putStrLn $ T.replicate 50 "="
      putStrLn $ "Finished: " ++ show a
      T.putStrLn $ T.replicate 50 "="
