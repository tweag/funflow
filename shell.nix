{ nixpkgs ? ./nix/default.nix
, ghcide ? false
, haskellLanguageServer ? true
, python-language-server ? true
}:
let
  pkgs = import nixpkgs { };

  # Fetch from GitHub
  devShellsInputs =
    import
      (builtins.fetchGit {
        url = "git@github.com:tweag/nix-dev-shells.git";
        name = "nix-dev-shell";
        rev = "9dbca810bc4dd243fc5a62bd0eef81898798e286";
      })
      # Use your own version of nixpkgs
      { inherit pkgs; }
  ;

in
pkgs.mkShell {
  buildInputs = with devShellsInputs;
    # Common packages (e.g. tmate, git, ...)
    (common { })
    # Vim
    ++ (vim { languageClient = true; languageClientOptions = { inherit ghcide haskellLanguageServer python-language-server; }; })
    # Standard Haskell dev environment
    ++
    (
      haskell {
        inherit haskellLanguageServer;
        haskellLanguageServerGhcVersion = "ghc884";
        ghcide = false; 
      }
    )
    # Python dev environment with packages
    ++
    (
      python {
        withPackages =
          pp:
          [
            # Needs fixing
            # pp.pytorchWithoutCuda
            # pp.torchvision
          ]
        ;
      }
    )
    # Custom
    ++
    [
      pkgs.cabal-install
      pkgs.haskell.packages.ghc883.inliterate
      pkgs.docker
      pkgs.zlib
      pkgs.git
      pkgs.cabal-install
    ]
  ;

  NIX_PATH = "nixpkgs=${nixpkgs}";
}
