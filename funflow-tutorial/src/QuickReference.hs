{-# OPTIONS_GHC -F -pgmF inlitpp #-}

# funflow2: Quick reference

This document presents how to use the API through short examples.

This document uses the following language extensions:

```haskell top
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
```

```haskell top hide
-- Add the instances to display 'haskell eval' IO results
import Lib ()
```


All imports are available in the `Funflow` module:

```haskell top
import Funflow
```

## A minimal flow

```haskell eval
let
  flow :: Flow String String
  flow = pureFlow $ \input -> "Hello " ++ input ++ " !"
  
  input :: String
  input = "Watson"
in
  runFlow flow input :: IO String
```


### Composing flows

```haskell eval
let
  flow1 :: Flow () String
  flow1 = pureFlow $ \input -> "Hello"

  flow2 :: Flow String String
  flow2 = pureFlow $ \input -> input ++ " world"
  
  flow :: Flow () String
  flow = flow1 >>> flow2
in
  runFlow flow () :: IO String
```

### Conditional branching

```haskell eval
let
  increment :: Flow Int Int
  increment = pureFlow (+ 1)

  reset :: Flow Int Int
  reset = pureFlow (const 0)

  limitedIncrement :: Flow Int Int
  limitedIncrement = proc n -> do
    if n < 10
      then do increment -< n
      else reset -< n

  flow :: Flow Int Int
  flow = limitedIncrement >>> limitedIncrement >>> limitedIncrement
in
  do
    runFlow flow (9 :: Int) :: IO Int
```

### Caching a flow

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
    runFlow flow1 (0 :: Int) :: IO Int
    -- Prints "Increment!" once to stdout
    runFlow flow2 (0 :: Int) :: IO Int
    return ()
```
