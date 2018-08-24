module Main where

import Control.DeepSeq

import Test.Tasty
  ( testGroup
  , defaultMain
  , TestTree
  )

import Test.Tasty.HUnit
import System.Directory

import Control.Funflow.CWL.Run ( tryRun, Args (..), UseCoord (..) )
import Data.HList
import Data.ErrorMonad


main :: IO ()
main = do
  cwd <- getCurrentDirectory
  defaultMain allTests
  forceIO $ setCurrentDirectory cwd
  clean_up

  where
    clean_up :: IO ()
    clean_up = return ()

    allTests :: TestTree
    allTests = testGroup "All tests" tests



tests :: [TestTree]
tests =
  [ testGroup "Good /guide tests" goodGuide
--  , testGroup "Job-WF mismatch /guide tests" badGuide
  , testGroup "Custom /rabix Tests" rabix
  ]
  where
    goodGuide = map (testPair guideDir) guidePairs
--    badGuide = map (testBadJobPair "guide") guidePairsFail
    rabix = map (testPair rabixDir) rabixPairs
    guideDir = "/test/guide/"
    rabixDir = "/test/rabix/"



type TestDir = String

testPair :: TestDir -> Pair -> TestTree
testPair dir pair =
  let nm = "Testing: " ++ show pair in
  testCase nm $ mkGoodAssert dir pair

mkGoodAssert :: TestDir -> Pair -> Assertion
mkGoodAssert dir (cwl, job) = do
  cwd <- getCurrentDirectory
  setCurrentDirectory (cwd ++ dir)
  let args = localArgs cwl job
  result <- runIOErrM $ tryRun args
  forceIO $ setCurrentDirectory cwd
  case result of
    Left e -> do
      assertFailure $ show e
    Right (SomeHL hl) -> do
      putStrLn "Success:"
      putStrLn $ show hl


localArgs :: FilePath -> FilePath -> Args
localArgs cwlpath jobpath =
  Args cwlpath jobpath storepath UseMem where
    storepath = "cwlFunflowTest"


forceIO :: NFData a => IO a -> IO a
forceIO m = do
  x <- m
  return $ force x

{-

testBadJobPair :: TestDir -> Pair -> TestTree
testBadJobPair dir pair = 
  let nm = "Testing: " ++ show pair in
  testCase nm $ mkMisMatchAssrt dir pair


mkMisMatchAssrt :: TestDir -> Pair -> Assertion
mkMisMatchAssrt dir (cwl, job) = do
  let cwl' = prefix dir cwl
  let job' = prefix dir job
  cwd <- getCurrentDirectory
  setCurrentDirectory dir
  result <- runIOErrM $ tryRun cwl' job'
  case result of
    Left e -> do
      setCurrentDirectory cwd
      assertFailure $ show e
    Right (SomeHL hl) -> do
      setCurrentDirectory cwd
      putStrLn "Success:"
      putStrLn $ show hl
-}


-- # Lists of pairs
---------------------------------------------------------------------

type CWLFileNm = String
type CWLJobNm = String
type Pair = (CWLFileNm, CWLJobNm)

-- | These are pairs of cwl files and jobs
--   that come from the official cwl guide.
guidePairs :: [Pair]
guidePairs =
  [ ("1st-tool.cwl", "echo-job.yml")
  , ("inp.cwl", "inp-job.yml")
  , ("inp.cwl", "inp-job2.yml")
  , ("tar.cwl", "tar-job.yml")
  , ("stdout.cwl", "echo-job.yml")
  , ("tar-param.cwl", "tar-param-job.yml")
  , ("docker.cwl", "docker-job.yml")
  , ("arguments.cwl", "arguments-job.yml")
  , ("array-inputs.cwl", "array-inputs-job.yml")
  , ("array-outputs.cwl", "array-outputs-job.yml")
  , ("env.cwl", "echo-job.yml")
  , ("echo-pair.cwl", "echo-pair-job.yml")
  , ("scatter1.cwl", "scatter-job.yml")
  , ("scatter2.cwl", "scatter-job.yml")
  , ("1st-workflow.cwl", "1st-workflow-job.yml")
  ]



-- | These guide pairs are expected to fail at
--   the step where we connect the input job to the
--   converted flow.
guidePairsFail ::[Pair]
guidePairsFail =
  [ ("record.cwl", "record-job1.yml") -- Should say invalid job
  , ("record.cwl", "record-job2.yml") -- Should say invalid job
  , ("expression.cwl", "empty.yml") -- Uses JS expressions
  , ("createfile.cwl", "echo-job.yml") -- Uses initial work requirement.
  , ("record.cwl", "record-job3.yml")  -- Future work: records
  , ("nestedworkflows.cwl", "empty.yml") -- Future work: uses inline wf
  ]




-- | These are my hand-made examples using the rabix
--   GUI.
rabixPairs :: [Pair]
rabixPairs =
  [ ("compile.cwl", "compile-job.yml") -- stupid gcc error on my machine
  , ("compdocker.cwl", "compdocker-job.yml")
  , ("rename.cwl", "rename-job.yml")
  , ("simple-wf.cwl", "simple-wf-job.yml") -- stupid gcc error on my machine
  , ("touchfile.cwl", "touchfile-job.yml")
  , ("write-file.cwl", "write-file-job.yml")
  , ("argtest.cwl", "argtest-job.yml")
  , ("compileandexecute.cwl", "compileandexecute-job.yml")
  , ("simple-example.cwl", "simple-example-job.yml")
  ]






-- Tiny Utilities
---------------------------------------------------------------------

both :: (a -> b) -> (a,a) -> (b,b)
both f (x,y) = (f x, f y)

prefix :: String -> String -> String
prefix p str = p ++ str

