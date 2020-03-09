let
  iohk-overlay = builtins.fetchTarball
    https://github.com/input-output-hk/haskell.nix/archive/89e3e78719ccc944d220c2d3e5e6299052eecb82.tar.gz;
in
{ pkgs ? import ./nixpkgs-src.nix (import iohk-overlay)
}:
  pkgs.haskell-nix.stackProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./..; };
    modules = [
        # The SQLite tests break due to not finding 'echo'. We disable them for now.
        {packages.funflow.components.tests.unit-tests.testFlags = ["-p '! /SQLite/'"];}
    ];
  }
