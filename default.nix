# This is the main project default.nix
{ system ? builtins.currentSystem
, pkgs ? import ./nix/default.nix { inherit system; } }:
with pkgs; rec {
  # Libraries
  inherit funflow funflow-tests cas-store cas-hashable cas-hashable-s3
    docker-client docker-client-tests funflow-unit-tests;

  # Shell
  inherit funflow-shell;

  # Other executables
  inherit makefile-tool;

  # Documentation
  inherit api-docs generate-funflow-tutorials;
}
