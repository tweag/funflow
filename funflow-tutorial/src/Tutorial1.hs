{-# OPTIONS_GHC -F -pgmF inlitpp #-}

```haskell top hide
{-# LANGUAGE OverloadedStrings #-}

import Lib
import Funflow
```

# First steps with `funflow`

## Introduction

`funflow` is a library to programmatically author workflows.

Use `funflow` to author workflows as Direct Acyclic Graphs of tasks.

Backed by the powerful abstractions allowed by the Haskell language, `funflow` workflows can be themsleves used as tasks.
This allows to define modular workflows that you can compose together.
_Write less, do more!_

## From "workflows and tasks" to "flows"

In `funflow`, there is no distinction between a workflow and a task.
Indeed, we can compose tasks into bigger tasks, which would be called workflows, but then again it would be a task that is itself reusable to build bigger tasks.
Because we can no longer make a distinction between a task and a workflow, we prefer to use the word *flow*.

A flow is a task that takes an input and produces an output.
This library provides a unique and simple type to define flows:

```haskell
flow :: Flow input output
```

`input` and `output` are the types of the flow.
For instance a flow working on numbers might have the following type signature:

```haskell
flow :: Flow Int Int
```

## How to make flows

In order to build flows, use the functions defined in the module `Funflow.Flows` (also available in the module `Funflow`).
This module provides functions called _smart constructors_.
They facilitate building flows.

The most basic exemple is the *PureFlow*.
It allows to run any pure function (a function is pure if it does not have any side effect, such as reading a file or running a command).

For example, let us make a function that increments the input by 1.

```haskell top
flow :: Flow Int Int
flow = pureFlow (+1)
```

### Execute a flow

Everything needed to run flows is available in the module `Funflow.Run`.
In order to run a flow, use the function `runFlow`.
It is used as follow:

```haskell
runFlow config flow input
```

where

- `config` is the execution configuration, we will get to that later and use `defaultExecutionConfig` for now
- `flow` is the `Flow` to run
- `input` is the input, with the same type as the input type of `flow`

It will return a result of type `IO output` where `output` is the output type of `flow`.

Let's run our flow:

```haskell eval
runFlow defaultExecutionConfig flow (1 :: Int) :: IO Int
```

As expected, it returned 2.

You can explore the available smart constructors in the module `Funflow.Flows`.

