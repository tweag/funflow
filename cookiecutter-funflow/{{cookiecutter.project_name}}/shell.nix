{ pkgs ? import ./nix {}}:
with pkgs; 
  mkShell {
    buildInputs = [
      ghc
      stack
      zlib
      haskellPackages.ghcide
      docker
      # niv
    ];
  }
