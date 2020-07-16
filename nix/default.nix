let pkgs = import ./nixpkgs.nix { };
in pkgs.haskell-nix.stackProject {
  # 'cleanGit' cleans a source directory based on the files known by git
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "funflow-project";
    src = ./..;
  };
}
