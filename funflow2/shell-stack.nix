# This shell.nix is only used to provide stack with
# funflow's system dependencies for developer workflows. 
# Check out shell.nix for a dev shell.
let
  nixpkgs = import ./nix/default.nix { };
in
nixpkgs.funflow-shell
