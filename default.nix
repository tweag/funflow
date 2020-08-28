# This is the main project default.nix
{ system ? builtins.currentSystem
, pkgs ? import ./nix/default.nix { inherit system; }
}:
let
  # These Haskell libraries will get bundled into the API documentation
  doc-libs = with pkgs; [
    funflow
    funflow-tests
    cas-store
    cas-hashable
    cas-hashable-s3
    external-executor
  ];
in
with pkgs; rec {
  # Libraries
  inherit funflow funflow-tests cas-store cas-hashable cas-hashable-s3 external-executor;

  # Shell
  inherit funflow-shell;

  # Tutorial exes (this is a set and each attribute is a funflow-tutorial executable)
  inherit funflow-tutorial;

  # Documentation
  api-docs = haddock-combine { hspkgs = doc-libs; };
  tutorial-docs = pkgs.generate-funflow-tutorials;
  # Combined API Docs + Tutorials
  combined-docs = pkgs.symlinkJoin { name = "funflow-combined-docs"; paths = [ api-docs pkgs.generate-funflow-tutorials ]; };
}
