{-# OPTIONS_GHC -F -pgmF inlitpp #-}

```haskell top hide
{-# LANGUAGE OverloadedStrings #-}

import Lib ()
```

# funflow2: Quick reference

This document presents how to use the API through short examples.

All imports are available in the `Funflow` module:

```haskell top
import Funflow
```

## 1. A minimal flow

```haskell eval
    let
      -- A flow from a pure function
      -- which takes a `String` as input and outputs a `String`
      flow :: Flow String String
      flow = pureFlow $ \input -> "Hello " ++ input ++ " !"
      
      -- Some input to run the flow on
      input :: String
      input = "Watson"
    in
      -- Run the flow with given input
      -- Returns an `IO output`, here `output` is `String`
      runFlow defaultExecutionConfig flow input :: IO String
```


### 2. Composing flows

```haskell eval
    let
      -- Two flows
      flow1 :: Flow () String
      flow1 = pureFlow $ \input -> "Hello"

      flow2 :: Flow String String
      flow2 = pureFlow $ \input -> input ++ " world"
      
      -- Combine both flows using `>>>`
      flow :: Flow () String
      flow = flow1 >>> flow2
    in
      runFlow defaultExecutionConfig flow () :: IO String
```

### 3. Running a shell command

```haskell eval
    let
      -- Prints "Hello world" to stdout and produces no output
      flow :: Flow () ()
      flow = shellFlow "echo Hello world"
    in
      runFlow defaultExecutionConfig flow () :: IO ()
```

### 4. Caching a flow

```haskell eval
    let
      -- Prints "Increment!" everytime it runs
      increment :: Flow Int Int
      increment = ioFlow $ \input -> do
        putStrLn "Increment!"
        return $ input + 1
      -- Reset any count to zero
      reset :: Flow Int Int
      reset = pureFlow $ \input -> 0
      -- Caching the `increment` flow
      cachedIncrement :: Flow Int Int
      cachedIncrement = caching ("increment" :: String) increment
      -- Without caching
      flow1 :: Flow Int Int
      flow1 = reset >>> increment >>> reset >>> increment
      -- With caching
      flow2 :: Flow Int Int
      flow2 = reset >>> cachedIncrement >>> reset >>> cachedIncrement
    in
      do
        -- Prints "Increment!" twice to stdout
        runFlow defaultExecutionConfig flow1 (0 :: Int) :: IO Int
        -- Prints "Increment!" once to stdout
        runFlow defaultExecutionConfig flow2 (0 :: Int) :: IO Int
        return ()
```

### Configuration Option

### Controlling Effects

