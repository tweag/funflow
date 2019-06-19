{ pkgs ? import ./nixpkgs-src.nix {} }:

let
  haskell = import (./haskell.nix-src.nix) { inherit pkgs; };

  pkgSet = haskell.mkStackPkgSet {
    stack-pkgs = import ./pkgs.nix;
    pkg-def-extras = [];
    modules = [
      {
        # The SQLite tests break due to not finding 'echo'. We disable them for now.
        packages.funflow.components.tests.unit-tests.testFlags = ["-p '! /SQLite/'"];
      }
    ];
  };

in
  pkgSet.config.hsPkgs
