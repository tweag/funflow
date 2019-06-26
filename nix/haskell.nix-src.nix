{ pkgs }:

let
  spec = builtins.fromJSON (builtins.readFile ./haskell.nix-src.json);
  haskell-nix-src = pkgs.fetchgit {
    name = "haskell-lib";
    inherit (spec) url rev sha256 fetchSubmodules;
  };
in
  import haskell-nix-src { inherit pkgs; }