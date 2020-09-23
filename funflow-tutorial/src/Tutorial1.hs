{-# OPTIONS_GHC -F -pgmF inlitpp #-}

```haskell top hide
{-# LANGUAGE OverloadedStrings #-}

import Lib
import Funflow
```

# First steps with `funflow`

## Introduction

`funflow` is a library to programmatically author workflows.

Use `funflow` to write workflows as Direct Acyclic Graphs (DAG) of tasks.

Backed by the powerful abstractions allowed by the Haskell language, `funflow` workflows have the great property of being tasks themselves.
This allows to define modular workflows that you can compose together.
_Write less, do more!_

## From "workflows and tasks" to "flows"

In `funflow`, there is no distinction between a workflow and a task.
Indeed, we can compose tasks into bigger tasks, which would be called workflows.
However, this workflow is nothing more than a task, which is then reusable to build bigger workflows.
Because we can no longer make a distinction between a task and a workflow, we prefer to use the word _flow_.

A flow is a task that takes an input and produces an output.
This library provides a unique and simple type to define flows:

```haskell
flow :: Flow input output
```

`input` and `output` are the types of the input and output values of the flow.
For instance a flow working on numbers might have the following type signature:

```haskell
flow :: Flow Int Int
```

taking an integer as input, and producing an integer as its output.

## How to make flows

In order to build flows, use the function `toFlow` defined in the module `Funflow.Flow` (also exported in the module `Funflow`).
This function can turn a _task_ into a `Flow`.

But what is a task?
A task is basically the representation of a computation.
A task is not per-se usable as a `Flow`: we have to _strand_ it to a flow manually using this `toFlow` function.

> We'll get to that notion of _strand_ later on, when extending our flow with custom tasks.

The most basic exemple is the _PureTask_.
It represent a computation from a pure function, which has not "side task" such as reading a file or running a command.

For example, let us make a function that increments the input by 1.

```haskell
flow :: Flow Int Int
flow = toFlow . PureTask $ (+1)
```

#### Smart constructors

All tasks internally implemented in `funflow` can be created using _smart constructors_.

For instance instead of the previous

```haskell
flow :: Flow Int Int
flow = toFlow . PureTask $ (+1)
```

one can write

```haskell top
flow :: Flow Int Int
flow = pureFlow (+1)
```

this directly makes a flow: the task is created and _stranded_ internally (we will talk about strands later on).

### Execute a flow

Everything needed to run flows is available in the module `Funflow.Run`.
In order to run a flow, use the function `runFlow`.
It is used as follow:

```haskell
runFlow flow input
```

where

- `flow` is the `Flow` to run
- `input` is the input, with the same type as the input type of `flow`

It will return a result of type `IO output` where `output` is the output type of `flow`.

Let's run our flow:

```haskell eval
runFlow flow (1 :: Int) :: IO Int
```

As expected, it returned 2.

### Available tasks

Available tasks are defined in `Funflow.Tasks`, and their smart constructors are defined in `Funflow.Flow`
