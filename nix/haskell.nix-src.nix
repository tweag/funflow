{ fetchgit, ... }:

let
  spec = builtins.fromJSON (builtins.readFile ./haskell.nix-src.json);
  haskell-iohk-overlay = fetchgit {
    name = "haskell-lib";
    inherit (spec) url rev sha256 fetchSubmodules;
  };
in
  import haskell-iohk-overlay
