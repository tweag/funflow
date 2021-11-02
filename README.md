# Funflow

Compose and run computational workflows.

> Looking for the previous major version of funflow? It can be found
> [here](https://github.com/tweag/funflow/tree/funflow1).

## Introduction

`funflow` is a Haskell library to write workflows programmatically, using
[kernmantle](https://github.com/tweag/kernmantle/) under the hood to model
workflows using binary effects.

Funflow allows you to compose tasks into *reusable* workflows and helps
promote reproducibility by employing a content-addressed store.

https://tweag.github.io/funflow/

## Getting started

### Installation

#### Cookiecutter Template

To get started with a simple project, you can use the cookiecutter template
provided with this repo:

```console
cookiecutter git@github.com:tweag/funflow.git --directory cookiecutter-funflow
```

#### Stack

While Funflow 2 hasn't been plublished on Hackage yet, you can install it as a git
dependency using the standard Haskell tooling (e.g.
[Stack](https://docs.haskellstack.org/en/stable/yaml_configuration/#packages)).
Note that you will also need to add a couple of extra dependencies to your `stack.yaml`.
See the [example stack.yaml](./cookiecutter-funflow/{{cookiecutter.project_name}}/stack.yaml) for a minimal example.

### Tutorials

Check out the tutorials on the funflow website to get started:
https://tweag.github.io/funflow/tutorials/.

You can run the tutorial notebooks using the nix shell provided in the
[funflow-tutorial](./funflow-tutorial) directory.

## Documentation

The API documentation can be found here: https://tweag.github.io/funflow/api/

## Developement

For those interested in contributing, please see
[DEVELOPMENT.md](./docs/DEVELOPMENT.md).
