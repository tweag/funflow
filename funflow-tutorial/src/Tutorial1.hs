{-# OPTIONS_GHC -F -pgmF inlitpp #-}

```haskell top hide
{-# LANGUAGE OverloadedStrings #-}

import Lib
import Funflow
import Funflow.Tasks.Simple (SimpleTask( PureTask, IOTask ))
```

# First steps with `funflow`

## Introduction

`funflow` is a Haskell library for defining and running _workflows_.
A workflow specifies a pipeline of _tasks_ structured in a Direct Acyclic Graph (DAG).
Workflows in `funflow` have the great property of being composable which means that  you can easily share and combine components across different workflows.
It supports type checking, result caching, and other features that simplify setting up your machinery.

_Let's get started_

## Anatomy of a Flow

In `funflow`, we refer to workflows as `flows`.
A `Flow` takes an input and produces an  output, and `funflow` describes it with a unique and simple type:

```haskell
flow :: Flow input output
```

`input` and `output` are the types of the input and output values of the flow.
For instance a flow working on numbers might have the following type signature:

```haskell
flow :: Flow Int Int
```

It takes an integer as input and produces an integer as its output.
A flow that doesn't take any input can be written as:

```haskell
flow :: Flow () Int
```

Such a flow might request some user input or download some data.


## Tasks

A `Flow` is a DAG comprising one or more `Tasks` which describe __what__ you would like to execute. 
`funflow` works with a wide range of task granularities.
A `Task` can be a simple Haskell function, a database query, a command to run in a Docker container, or more. 

There are several different types of tasks in Funflow, each describing a specific type of computation. 
Tasks are defined in the `Funflow.Tasks` subpackage.
The most basic task, the datatype `PureTask`, represents  a pure Haskell function which has no _side effects_ such as reading a file or running a command.
Other task datatypes include `IOTask`, which runs a Haskell function which can perform I/O (e.g. reading a file), and `DockerTask`, which runs a 
[Docker](https://docs.docker.com/get-docker/) container.


## How to create Flows

The function `toFlow` is used to construct a `Flow`.
It can be imported from the top level `Funflow` module and is defined in `Funflow.Flow`.
It integrates a `Task` into a `Flow` which can then be composed with other flows into a larger, final `Flow` DAG.

Here is a `Flow` that runs a `PureTask`, incrementing its input by 1.

```haskell top
flow :: Flow Int Int
flow = toFlow $ PureTask (+1)
```

In this example, `flow` is essentially a DAG with one node, `PureTask (+1)`. 
Here is a flow that runs a simple IO task which prints its input.

```
flow :: Flow String ()
flow = toFlow $ IOTask putStrLn
```

Single-task `Flows` like the ones above can also be created directly using their smart constructors.
For instance, instead of the previous, one can write:

```
flow :: Flow Int Int
flow = pureFlow (+1)
```

or

```
flow :: Flow String ()
flow = ioFlow putStrLn
```

Smart constuctors for other task types are defined in `Funflow.Flow`.

## Execute a flow

Everything needed to run flows is available in the module `Funflow.Run`.
The function `runFlow` is the main way to execute a flow:

```haskell
runFlow flow input
```

where

- `flow` is the `Flow` to run
- `input` is the input, with the same type as the input type of `flow`

It will return a result of type `IO output` where `output` is the output type of `flow`.
Let's run our flow from earlier:

```haskell eval
runFlow flow (1 :: Int) :: IO Int
```

As expected, it returned 2.

Astute readers may have noticed that the output of runFlow is of type `IO output` and not simply `output`.
This wrapping of `output` in `IO` happens because runFlow uses a context in which all possible Task types can be
executed. Since runFlow supports IO and Docker tasks, both of which utilize I/O, the output of runFlow is also of type `IO`.

## Next Steps

With the basics out of the way, you should be ready to start writing your first `flows`. Check out the [wordcount flow tutorial](./wordcount.html)
for a guided example.
