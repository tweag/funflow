# Provide Nix support to Stack by expressing system packages required, rather than manually having to install stuff like Zlib
# Inspired by https://docs.haskellstack.org/en/stable/nix_integration/#using-a-custom-shellnix-file
{ ghc, pkgs ? import ./pkgs.nix }:

pkgs.haskell.lib.buildStackProject {
  inherit ghc;
  name = "funflow-project";
  buildInputs = [ pkgs.zlib ];
}
