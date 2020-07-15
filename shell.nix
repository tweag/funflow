{...}@args:
let
  nixpkgs = ./nix/nixpkgs.nix;
  pkgs = import nixpkgs {};
  project = import ./nix;
in
  project.shellFor
    (
      {
        buildInputs = with pkgs;
          [
            nix
            git
            cabal-install
            zlib
            docker
          ];
        
        exactDeps = true;

        STACK_IN_NIX_SHELL = true;
        NIX_PATH="${<nixpkgs>}";
      }
    )
