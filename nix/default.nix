let
  iohk-overlay = import ./haskell.nix-src.nix;
in
{ pkgs ? import ./nixpkgs-src.nix iohk-overlay
}:
  pkgs.haskell-nix.stackProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./..; };
    modules = [
        # The SQLite tests break due to not finding 'echo'. We disable them for now.
        {packages.funflow.components.tests.unit-tests.testFlags = ["-p '! /SQLite/'"];}
    ];
  }
