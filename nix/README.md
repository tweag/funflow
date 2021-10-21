# Updating `haskell.nix` version
```console
niv update haskellNix
```

# Building with Nix (e.g. for CI)
## Build Funflow library
```console
nix-build -A funflow.components.library
```
## Build unit tests
```console
nix-build -A funflow.components.tests.unit-tests
```

## Explore all build targets
### Other funflow targets
```console
nix-build -A funflow.components.<tab>
```
### Other targets (e.g. kernmantle or cas-hashable)
```console
nix-build -A <tab>
```
