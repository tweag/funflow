{-# OPTIONS_GHC -F -pgmF inlitpp #-}

# Example: WordCount

In this example, we'll implement a simple pipeline which 
calculates word frequencies in a plain text document. Our
example pipeline will make use of `PureEffect` and `IOEffect`, which 
allow us to define our pipeline's tasks in terms of Haskell functions. 

This example may look familiar to users of Apache Beam, which 
also includes a WordCount example https://beam.apache.org/get-started/wordcount-example/.

## Imports

First, we'll need to import some additional modules which will enable us to 
more easily work with text (`Data.Text`), define dictionaries/maps (`Data.Map`), perform
regex matching (`Text.Regex.Posix`), and more.

```haskell top
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.List (sortBy)
import qualified Data.Map as Map
import Data.Ord (comparing)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Text.Printf (printf)
import Text.Regex.Posix ((=~))

import Funflow
```

```haskell top hide
import Lib ()
```

## Helper Functions

Since we're opting to write our pipeline using Haskell-based Flows, we need to define the Haskell
functions which will take care of parsing our input text and counting its words. 

```haskell top
-- | (word, n_occurences)
type TextCount = (T.Text, Int)

-- | Counts members in a list of text. Also works for lazy lists (e.g. data from readFile)
countWords :: [T.Text] -> [TextCount]
countWords ws = let
        tally :: Map.Map T.Text Int -> T.Text -> Map.Map T.Text Int
        tally m k = if Map.member k m 
                    then Map.adjust (+1) k m
                    else Map.insert k 1 m
    in 
        Map.toList $ foldl tally Map.empty ws

-- | Removes punctuation marks from a text
removePunctuation :: T.Text -> T.Text
removePunctuation = 
    let
        punctuation = ",.?!:;\"\'" :: String
    in T.filter (not . (`elem` punctuation))

-- | Filters words which are not comprised of latin characters (hyphens are allowed)
filterWords :: [T.Text] -> [T.Text]
filterWords = 
    let 
        wordsRegex = "[A-Za-z\']+" :: String
    in filter $ (=~ wordsRegex) . T.unpack

-- | Like countWords, but with sorted results
countWordsSortedDesc :: [T.Text] -> [TextCount]
countWordsSortedDesc = sortCountsDesc . countWords

-- | Sorts a list of word counts in descending order
sortCountsDesc :: [TextCount] -> [TextCount]
sortCountsDesc = sortBy (flip $ comparing snd)

-- | Prepare word counts for printing
formatCounts :: [TextCount] -> [T.Text]
formatCounts = map (\(w,c) -> T.pack $ printf "%s: %d" (T.unpack w) c)
```

## Pipeline Definition

With our core utility functions defined, we're ready to define our pipeline. One 
simple way to structure our pipeline is to divide it into three tasks/operations:

1. Read the input text file 
2. Parse the input text and return a summary of the word frequencies in it 
3. Write out our results. For this example, we can just write them directly to the terminal.

Remember that in Funflow both tasks and a pipeline/DAG are described in terms of the same 
type, a Flow. This is in contract to applications like Airflow, which separate out 
tasks and pipelines into Operator and DAG objects. 

```haskell top
-- Individual task definitions (remember that each task is also a full "Flow")
readDocument :: Flow String T.Text
readDocument = toFlow . IOEffect $ T.readFile

countWordsAndSummarize :: Flow T.Text T.Text
countWordsAndSummarize = toFlow . PureEffect $ (T.unlines . formatCounts . countWordsSortedDesc . filterWords. T.words . removePunctuation)

writeResult :: Flow (String, T.Text) ()
writeResult = let
        writeOutputMessage :: (String, T.Text) -> IO ()
        writeOutputMessage (f, countText) = do
            T.putStrLn "Normally we would write the result to a file with T.writeFile, but for this example we can instead print the output:"
            T.putStrLn countText 
            return ()
    in toFlow . IOEffect $ writeOutputMessage

-- Build the final pipeline using the task Flows defined above
--   Note: Using arrow syntax to control which inputs get passed to
--   which pipeline tasks, i.e. result_name <- task <- task_input
flow :: Flow (String, String) ()
flow = proc (documentFilePath, outputSummaryFilePath) -> do
    documentText <- readDocument -< documentFilePath
    countSummary <- countWordsAndSummarize -< documentText
    writeResult -< (outputSummaryFilePath, countSummary)
```

## Run the pipeline

And finally, with our pipeline defined, we're ready to run it!

```haskell eval
runFlow defaultExecutionConfig flow ("words.txt":: String, "outputs/counts.txt"::String) :: IO ()
```
