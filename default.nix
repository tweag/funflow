# Copied and adapted from https://input-output-hk.github.io/haskell.nix/tutorials/getting-started/#scaffolding
let pkgs = import ./nix/pkgs.nix;
in pkgs.haskell-nix.project {
  # 'cleanGit' cleans a source directory based on the files known by git
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "funflow-project";
    src = ./.;
  };
}
