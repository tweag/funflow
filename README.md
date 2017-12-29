FunFlow
========

[![CircleCI](https://circleci.com/gh/tweag/funflow.svg?style=svg)](https://circleci.com/gh/tweag/funflow)

FunFlow is a workflow framework for Haskell based on [arrows](https://www.haskell.org/arrows/). Individual steps are written as simple monadic functions (`a -> IO b`) and these are glued together with the arrow combinators or arrow syntax. Unlike many other workflow frameworks, steps have inputs and outputs and their wiring is used to automatically deduce dependencies. Thus, explicit declaration dependencies and contexts are no longer necessary.

FunFlow provides:

* Sequence, parallel, choice and loops in workflows
* Export of the dependency graph
* Resume computations after failure or interruption, with or without recompilation
* Task logging

A workflow in FunFlow has the type `Flow a b` indicating a workflow that takes an input of type `a` and has a final result type `b`. Individual steps and combinations of steps that have been wired together with arrows have the same type. This way, increasingly complex workflows can be composed of small units of combines steps.

## Steps

Individual effectful steps are created using the `Step` combinator:

`Step :: (Store a, Store b) => (a -> IO b) -> Flow a b`

The inputs and the outputs have to be in the `Store` class defined in the `store` package. Many types are already defined in this type class so it is quite likely that you won't have to do any work in that respect. 

Pure computations can be raised into workflows using the standard arrow combinators, for instance `arr :: (a -> b) -> Flow a b`. Pure computations turned into workflows in this way will not be cached in the same way as steps based on monadic functions declared using `Step`. This is because any type can be used in a pure computation, including types that are not in the `Store` type class and thus we don't know how to serialise them to a binary store. Therefore pure computations may needlessly be repeated if the workflow is restarted.

If you would like to cache a pure computation in the same way as an effectful `Step`, you can give it a name:

`Name :: (Store a, Store b) => Text -> Flow a b -> Flow a b`

Both the input and the output types of named workflows are subject to the same restrictions as steps, that is they have to be in the Store type class. Naming workflows has two effects:

* Named workflows are cached so they will not be repeated if the calculation is restarted. 
* Names given to workflows help when you are logging or visualising the dependency graph

## Wiring 

Workflows are wired together (*composed*) using the arrow combinators or the arrow syntax.

Example: 

```haskell
myFlow :: Flow () Bool
myFlow = proc () -> do
  age <- Name "getName" promptFor -< "How old are you"
  returnA -< age > (65::Int)
```

Helper functions and some basic steps are given in the module [Control.FunFlow.Steps](https://github.com/glutamate/funflow/blob/master/src/Control/FunFlow/Steps.hs)

You can implement your own retry logic (or use the simple one in the Control.FunFlow.Steps module) based on the elementary combinators for failure handling. See `Control.FunFlow.Steps.retry`.

## Execution engines

Workflows can be executed in a variety of ways and you can define your own interpreters for workflows with different notions of persistence. Three execution engines are included in the `funflow` package:

* A basic non-interruptible interpreter `runFlow` is given in [Control.FunFlow.Base](https://github.com/glutamate/funflow/blob/master/src/Control/FunFlow/Base.hs)
* An execution engine based on Redis is given in [Control.FunFlow.Exec.Redis](https://github.com/glutamate/funflow/blob/master/src/Control/FunFlow/Exec/Redis.hs)
