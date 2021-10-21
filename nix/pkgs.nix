# Copied and adapted from https://input-output-hk.github.io/haskell.nix/tutorials/getting-started/#scaffolding
let
  # Read in the Niv sources
  sources = import ./sources.nix { };

  # Fetch the haskell.nix commit we have pinned with Niv
  haskellNix = import sources.haskellNix { };

  # Import nixpkgs and pass the haskell.nix provided nixpkgsArgs
  pkgs = import
    # haskell.nix provides access to the nixpkgs pins which are used by our CI,
    # hence you will be more likely to get cache hits when using these.
    # But you can also just use your own, e.g. '<nixpkgs>'.
    haskellNix.sources.nixpkgs-unstable
    # These arguments passed to nixpkgs, include some patches and also
    # the haskell.nix functionality itself as an overlay.
    haskellNix.nixpkgsArgs;
in pkgs
