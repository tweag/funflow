#!/usr/bin/env bash

nix build -f https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz nix-tools --out-link nt
./nt/bin/stack-to-nix -o nix

unlink nt