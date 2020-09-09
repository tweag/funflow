# funflow

Compose and run computational workflows.

## Introduction

`funflow` is a Haskell library to write workflows programatically.

- Compose tasks to make reusable workflows
- Improve reproducibility by using a content-addressed store

## Getting started

Add `funflow` as a dependency to your Haskell project.

Then in a main module:

```haskell
import Funflow
import Funflow.Effects.Docker

-- Say "Hello world" in a docker container
flow = dockerFlow $ DockerEffectConfig {image = "alpine", command = "echo", args = ["Hello world"]}

main :: IO ()
main = runFlow flow (DockerEffectInput {DE.inputBindings = mempty, DE.argsVals = mempty}) >> return ()
```

This will start a docker container, print `"Hello world"` and exit.

[Learn more](https://tweag.github.io/funflow2/tutorial/tutorial1)

## From tasks and workflows to _flows_

In the existing libraries to make workflows, there is always a distinction between tasks and workflows: a task describes something to do and a workflow describes in which order the tasks needs to be run.
In this setting, there is no notion of input or output to tasks, and a workflow is just a directed acyclic graph (DAG) that describes how tasks are _dependent_ on one each other.

In `funflow`, tasks are computations that take inputs and produce outputs.
Building a workflow is not just a matter of sequencing operations through time, but _composing them_.
Indeed, by saying that a task will consume the output of another task, the dependency relationship becomes natural.

This allows to represent workflows as the composition of tasks... which itself is a task that can be reused!

Since tasks and workflows become the same thing, we simply call them **flows**.

## Documentation

The API documentation can be found here: https://tweag.github.io/funflow2/doc/funflow/html/Funflow.html

### Tutorials

- First steps with `funflow`: [read](https://tweag.github.io/funflow2/tutorial/tutorial1)
- "Word count" example : [read](https://tweag.github.io/funflow2/tutorial/wordcount)
- Quick reference : [read](https://tweag.github.io/funflow2/tutorial/quick-reference)

## Developement

See [DEVELOPMENT.md](DEVELOPMENT.md).
