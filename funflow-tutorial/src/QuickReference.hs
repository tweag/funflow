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
      flow :: Flow String String
      flow = pureFlow $ \input -> "Hello " ++ input ++ " !"
      
      input :: String
      input = "Watson"
    in
      runFlow defaultExecutionConfig flow input :: IO String
```


### 2. Composing flows

```haskell eval
    let
      flow1 :: Flow () String
      flow1 = pureFlow $ \input -> "Hello"

      flow2 :: Flow String String
      flow2 = pureFlow $ \input -> input ++ " world"
      
      flow :: Flow () String
      flow = flow1 >>> flow2
    in
      runFlow defaultExecutionConfig flow () :: IO String
```

### 3. Running a shell command

```haskell eval
    let
      flow :: Flow () ()
      flow = shellFlow "echo Hello world"
    in
      runFlow defaultExecutionConfig flow () :: IO ()
```

### 4. Caching a flow

```haskell eval
    let
      increment :: Flow Int Int
      increment = ioFlow $ \input -> do
        putStrLn "Increment!"
        return $ input + 1

      reset :: Flow Int Int
      reset = pureFlow $ \input -> 0

      cachedIncrement :: Flow Int Int
      cachedIncrement = caching ("increment" :: String) increment

      flow1 :: Flow Int Int
      flow1 = reset >>> increment >>> reset >>> increment

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

