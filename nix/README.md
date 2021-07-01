# nix/

This directory contains the nixpkgs definition for this project. To import the nixpkgs used by funflow, import the [default.nix](./default.nix) from this directory.
To access funflow's nix attributes, use the repo-level [default.nix](../default.nix).

This project uses [haskell.nix](https://github.com/input-output-hk/haskell.nix) and its `stackProject` function to build all of its Haskell libraries.
