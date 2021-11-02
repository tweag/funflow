# Development

<!-- toc -->

- [Requirements](#requirements)
- [Development environment](#development-environment)
    - [VS Code](#vs-code)
    - [Install and start Docker](#install-and-start-docker)
    - [Optional: Enable Direnv](#optional-enable-direnv)
- [Build with `stack`](#build-with-stack)
- [Updating the Nix dependencies](#updating-the-nix-dependencies)

<!-- tocstop -->

## Requirements

- [Nix](https://nixos.org/nix/)
- We recommend that you make use of [Cachix](https://cachix.org/) to speed up your builds.

  Once you have installed `cachix`, configure it to use this project's cache:

  ```bash
  cachix use funflow
  ```

## Development environment

A [shell.nix](../shell.nix) file is available to provide with the minimum environment to develop and build the library.

```bash
nix-shell
```

Non-exaustive list of things provided by the shell:
* [Stack](https://docs.haskellstack.org/en/stable/README/) (with Nix integration preconfigured)
* [Haskell Language Server](https://github.com/haskell/haskell-language-server) (HLS) which can be used with any LSP-compatible editor to improve the Haskell development experience (code navigation, completion, typechecking, etc.)
* [Ormolu](https://hackage.haskell.org/package/ormolu) formatter

### VS Code

Plugin requirements:

- [Nix Environment Selector](https://marketplace.visualstudio.com/items?itemName=arrterian.nix-env-selector)
  - select the `shell.nix` file
- [Haskell](https://marketplace.visualstudio.com/items?itemName=haskell.haskell)
  - HLS is provided by the development Nix shell

### Install and start Docker
For tasks like running tests and executing some of the [tutorials](../funflow-tutorial), Docker should be up and running on your machine. Check out the official Docker website for more information about [installation](https://docs.docker.com/engine/install/), and perhaps about the [Docker daemon](https://docs.docker.com/config/daemon/).

### Optional: Enable Direnv
Direnv additionally provides auto-loading when navigating into directories in shells. Thus you can rely on it to automatically load `shell.nix` whenever you `cd` to Funflow (and/or when you navigate to the `funflow-tutorial` subdirectory which has another `shell.nix`) while keeping your favorite shell (ZSH, Bash, Fish...).

* install globally [direnv](https://direnv.net/) (e.g. `nix-env --install direnv`)
* install globally [nix-direnv](https://github.com/nix-community/nix-direnv)  (e.g. `nix-env --install nix-direnv`)
* create a file `.envrc` at the root of Funflow containing `use nix`
* enable `direnv` integration in Funflow directory: `direnv allow` (and do the same)
* repeat 2 previous steps for `funflow-tutorial/` subdirectory
* now whenever you `cd` into a Funflow directory or sub-directory, your shell will be configured with dependencies as if you were in `nix-shell`.

## Build with `stack`

All the usual `stack` commands should work out of the box, that is:

* Build
  ```bash
  stack build funflow
  ```
* Run tests
  ```bash
  stack test
  ```
* Launch a REPL (Read-Eval-Print-Loop, also called GHCI in Haskell lingo):
  ```bash
  stack repl
  ```

## Updating the Nix dependencies

If you want to use more recent packages (e.g a more recent GHC version by updating the stackage resolver, or a more recent Haskell Language Server), you may need to update the Nix dependencies:

```bash
niv update
```
