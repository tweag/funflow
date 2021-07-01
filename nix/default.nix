{ system ? builtins.currentSystem }:
let
  overlays = import ./overlays.nix;
  pkgs = import ./nixpkgs.nix { overlays = overlays; system = system; };
in
pkgs
