let
  # Read in the Niv sources
  sources = import ./sources.nix { };

  overlays = import ./overlays.nix;

  pkgs = import sources.nixpkgs { inherit overlays; };
  # This is the main Nixpkgs package, that should be imported and used by all other Nix files that need some Nix package
  # Syntax:
  # let pkgs = import nix/pkgs.nix in ...
in pkgs
