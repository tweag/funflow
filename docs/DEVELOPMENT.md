# Development

<!-- toc -->

- [Requirements](#requirements)
- [Development environment](#development-environment)
  * [VS Code](#vs-code)
  * [Install and start Docker](#install-and-start-docker)
- [Build with `stack`](#build-with-stack)
- [CI](#ci)
  * [Updating the Nix build](#updating-the-nix-build)
  * [Build locally](#build-locally)

<!-- tocstop -->

## Requirements

- [Nix](https://nixos.org/nix/)
- We recommend that you make use of [Cachix](https://cachix.org/) to speed up your builds.

  Once you have installed `cachix`, configure it to use this project's cache:

  ```bash
  cachix use funflow
  ```

## Development environment

A `shell.nix` file is available to provide with the minimum environment to develop and build the library.

```bash
nix-shell shell.nix
```

A `shell-stack.nix` file is available to provide the suitable environment to build the project, which is the environment in which `stack` makes its builds.

A `hie.yaml` file is available at the root of the project to use Haskell Language Server.
HLS can be used with VS Code/vim/emacs to improve the development experience.

### VS Code

Requirements:

- [Nix Environment Selector](https://marketplace.visualstudio.com/items?itemName=arrterian.nix-env-selector)
  - select the `shell-dev.nix` file
- [Haskell](https://marketplace.visualstudio.com/items?itemName=haskell.haskell)
  - HLS is provided by the development Nix shell

### Install and start Docker
For tasks like running tests and executing some of the [tutorials](../funflow-tutorial), Docker should be up and running on your machine. Check out the official Docker website for more about [installation](https://docs.docker.com/engine/install/), and perhaps about the [Docker daemon](https://docs.docker.com/config/daemon/).

## Build with `stack`

Build the `funflow` library:

```bash
stack build funflow
```

Run tests:

```bash
stack test funflow
```

## CI

### Updating the Nix build

Funflow uses a Nix build based on [haskell.nix](https://github.com/input-output-hk/haskell.nix) for CI.

If you make major changes, such as updating the stackage resolver, you may need to move to a newer version of nixpkgs and the haskell.nix tooling:

```bash
./nix/update-nixpkgs.sh
./nix/regenerate.sh
```

### Build locally

Build the library:

```bash
nix-build nix -A funflow
```

Run the tests:

```bash
(cd funflow && "$(nix-build ../nix -A funflow-tests)/bin/test-funflow")
```

Build the API docs to a `result-api-docs` folder:

```bash
nix-build -o result-api-docs api-docs
```
