{ symlinkJoin, jupyterWith, docker }:

let
  pkgs = import ./pkgs.nix;
  compiler = "ghc8107";
  sources = import ./sources.nix { };

  # Jupyter - Nix integration #
  # See https://github.com/tweag/jupyterWith for standard Jupyter - Nix integration documentation
  # See https://github.com/tweag/jupyterWith/tree/master/kernels/ihaskell for iHaskell configuration in particular

  # Override the Haskell package set to use the current Funflow (and Kernmantle) code, rather than Hackage versions (so that tutorials use your current Funflow code)
  # store/store-core are included here simply because the Github version has outpaced the Hackage version, and supports text >= 2.0
  # As those are not "nixified", use `callCabal2nix` to convert them
  haskellPackages = pkgs.haskell.packages."${compiler}".override {
          overrides = hself: hsuper: rec {
            text = pkgs.haskell.lib.dontCheck hsuper.text_2_0_1;
            hashable = pkgs.haskell.lib.dontCheck hsuper.hashable_1_4_1_0;
            conduit-extra = pkgs.haskell.lib.dontCheck hsuper.conduit-extra;
            parsec = pkgs.haskell.lib.dontCheck hsuper.parsec_3_1_15_1;
            lens = pkgs.haskell.lib.dontCheck hsuper.lens_5_2;
            aeson = pkgs.haskell.lib.dontCheck hsuper.aeson_2_1_0_0;
            lens-aeson = pkgs.haskell.lib.dontCheck hsuper.lens-aeson_1_2_2;
            store = pkgs.haskell.lib.dontCheck (hself.callCabal2nix "store" "${sources.store}" { });
            store-core = pkgs.haskell.lib.dontCheck (hself.callCabal2nix "store-core" "${sources.store}/store-core" { });
            funflow = pkgs.haskell.lib.dontCheck (hself.callCabal2nix "funflow" ../funflow { });
            docker-client = pkgs.haskell.lib.dontCheck (hself.callCabal2nix "docker-client" ../docker-client { });
            cas-hashable = pkgs.haskell.lib.dontCheck (hself.callCabal2nix "cas-hashable" ../cas/hashable { });
            cas-store = pkgs.haskell.lib.dontCheck (hself.callCabal2nix "cas-store" ../cas/store { });
            kernmantle = hself.callCabal2nix "kernmantle" "${sources.kernmantle}/kernmantle" { };
            kernmantle-batteries = hself.callCabal2nix "kernmantle-batteries" "${sources.kernmantle}/batteries" { };
            kernmantle-caching = hself.callCabal2nix "kernmantle-caching" "${sources.kernmantle}/caching" { };
          };
        };
  # Haskell packages available in the tutorial notebooks environment
  tutorialHaskellDependencies = p:
    with p; [
      funflow
      regex-posix
      text
      containers
      JuicyPixels
      ihaskell-juicypixels
      process
    ];

  iHaskell = jupyterWith.kernels.iHaskellWith {
    extraIHaskellFlags = "--codemirror Haskell";
    name = "haskell";
    inherit haskellPackages;
    packages = tutorialHaskellDependencies;
  };

in jupyterWith.jupyterlabWith {
  kernels = [ iHaskell ];
  extraPackages = p: [ p.docker ];
}
