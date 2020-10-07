# This is the main project default.nix
{ system ? builtins.currentSystem
, pkgs ? import ./nix/default.nix { inherit system; } }:
let
  # These Haskell libraries will get bundled into the API documentation
  doc-libs = with pkgs; [
    funflow
    funflow-tests
    cas-store
    cas-hashable
    cas-hashable-s3
    external-executor
    docker-client
  ];
in with pkgs; rec {
  # Libraries
  inherit funflow funflow-tests cas-store cas-hashable cas-hashable-s3
    external-executor docker-client docker-client-tests;

  # Shell
  inherit funflow-shell;

  # Tutorial exes (this is a set and each attribute is a funflow-tutorial executable)
  inherit funflow-tutorial;

  # Documentation
  api-docs = haddock-combine { hspkgs = doc-libs; };
  doc-index = generate-doc-index;
  inherit generate-funflow-tutorials;
}
