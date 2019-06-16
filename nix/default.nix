{ pkgs ? import ./nixpkgs-src.nix {} }:

let
  haskell = import (./haskell.nix-src.nix) { inherit pkgs; };

  pkgSet = haskell.mkStackPkgSet {
    stack-pkgs = import ./pkgs.nix;
    pkg-def-extras = [];
    modules = [];
  };

in
  pkgSet.config.hsPkgs
