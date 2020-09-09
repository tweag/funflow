# Development

## Requirements

- [Nix](https://nixos.org/nix/)
- We recommend that you make use of [Cachix](https://cachix.org/) to speed up your builds.

  Once you have installed `cachix`, configure it to use this project's cache:

  ```bash
  cachix use tweag-funflow2
  ```

## Development environment

A `shell-dev.nix` file is available to provide with the minimum environment to develop and build the library.

```bash
nix-shell shell-dev.nix
```

A `shell.nix` file is available to provide the suitable environment to build the project, which is the environment in which `stack` makes its builds.

A `hie.yaml` file is available at the root of the project to use Haskell Language Server.
HLS can be used with VS Code/vim/emacs to improve the development experience.

### VS Code

Requirements:

- [Nix Environment Selector](https://marketplace.visualstudio.com/items?itemName=arrterian.nix-env-selector)
  - select the `shell-dev.nix` file
- [Haskell](https://marketplace.visualstudio.com/items?itemName=haskell.haskell)
  - HLS is provided by the development Nix shell

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

Funflow uses a Nix build based on [[https://github.com/input-output-hk/haskell.nix][haskell.nix]] for CI.

If you make changes to the dependencies through modifying the `stack.yaml` or individual `.cabal` files, then you will need to regenerate the nix configuration by calling:

```bash
./nix/regenerate.sh
```

If you make major changes, such as updating the stackage resolver, you may need to move to a newer version of nixpkgs and the haskell.nix tooling:

```bash
./nix/update-nixpkgs.sh
./nix/regenerate.sh
```

### Build locally

Build the library:

```bash
nix-build nix -A funflow.components.library
```

Run the tests:

```bash
$(nix-build nix -A funflow.components.tests)/bin/test-funflow
```

Build the docs to a `result-docs` folder:

```bash
nix-build -o result-docs nix/docs
```
