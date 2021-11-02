{ symlinkJoin, jupyterWith, docker }:

let
  pkgs = import ./pkgs.nix;

  sources = import ./sources.nix { };

  # Jupyter - Nix integration #
  # See https://github.com/tweag/jupyterWith for standard Jupyter - Nix integration documentation
  # See https://github.com/tweag/jupyterWith/tree/master/kernels/ihaskell for iHaskell configuration in particular

  # Override the Haskell package set to use the current Funflow (and Kernmantle) code, rather than Hackage versions (so that tutorials use your current Funflow code)
  # As those are not "nixified", use `callCabal2nix` to convert them
  haskellPackages = pkgs.haskellPackages.override {
    overrides = hself: hsuper: {
      funflow = pkgs.haskell.lib.dontCheck
        (hself.callCabal2nix "funflow" ../funflow { });
      docker-client = pkgs.haskell.lib.dontCheck
        (hself.callCabal2nix "docker-client" ../docker-client { });
      kernmantle =
        hself.callCabal2nix "kernmantle" "${sources.kernmantle}/kernmantle" { };
      kernmantle-batteries = hself.callCabal2nix "kernmantle-batteries"
        "${sources.kernmantle}/batteries" { };
      kernmantle-caching =
        hself.callCabal2nix "kernmantle-caching" "${sources.kernmantle}/caching"
        { };
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
