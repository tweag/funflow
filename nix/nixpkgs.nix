# nixpkgs along with haskell.nix and any other specified overlays.
# Any additional nixpkgs args will be merged with the nixpkgs args from default.nix
{ system ? builtins.currentSystem
, overlays ? [ ]
, ...
} @ otherNixpkgsArgs:
let
  haskellNix = import ./haskell.nix-src.nix { };

  # haskell.nix provides access to the nixpkgs pins which are used by its CI,
  # hence you will be more likely to get cache hits when using these.
  # But you can also just use your own, e.g. '<nixpkgs>'.
  nixpkgsSrc = builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs/archive/1e3f09feaa5667be4ed6eca96a984b4642420b83.tar.gz";
    sha256 = "1vcmzjzzmcy4mvlyshv9j3cri4kvxndjqk3ziqafc8ik0vv117p8";
  };

  # haskell.nix provides some arguments to be passed to nixpkgs, including some
  # patches and also the haskell.nix functionality itself as an overlay. We
  # need to make sure that those overlays are included in the final set of nixpkgs args
  nixpkgsArgs = haskellNix.nixpkgsArgs // otherNixpkgsArgs // { overlays = haskellNix.nixpkgsArgs.overlays ++ overlays; system = system; };

  # import nixpkgs with haskell.nix overlays and custom args
  pkgs = import nixpkgsSrc nixpkgsArgs;
in
pkgs
