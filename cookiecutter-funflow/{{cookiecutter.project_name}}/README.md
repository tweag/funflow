# {{cookiecutter.project_name}}

Welcome to your funflow project! This project was created using the funflow cookiecutter template and is
intended to be used alongside the [Haskell Tool Stack (stack)](https://docs.haskellstack.org/en/stable/README/#how-to-install).

You may also get started with a development environment (including `stack`) using Nix. Simply install Nix then run the `nix-shell` command in this directory.

## Building your new project

You should be able to immediatebly build and execute your new project using stack. Simply run the following:

```console
$ stack build
```

then

```console
$ stack run

    "Mean of [1.0,2.0,3.0,4.0,5.0] is: 3.0"
```

## Writing your pipeline

This template generates a simple pipeline in [app/main.hs](app/main.hs). Modifying this pipeline can be a good starting point for your project.
