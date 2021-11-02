# All overlays for this project should be defined in the list below
let sources = import ./sources.nix { };
in [
  # Jupyter - Nix overlays
  # See https://github.com/tweag/jupyterWith#using-as-an-overlay
  (import "${sources.jupyterWith}/nix/haskell-overlay.nix")
  (import "${sources.jupyterWith}/nix/python-overlay.nix")
  (import "${sources.jupyterWith}/nix/overlay.nix")

  # Overlay for the tutorial Jupyter Lab environment
  (self: super: {
    funflow-tutorial-jupyter = super.callPackage ./jupyter.nix { };
  })

  # Documentation
  (self: super: {
    # Script for building tutorial html docs
    generate-funflow-tutorials = super.callPackage ./tutorials.nix {
      nbconvert = self.python3Packages.nbconvert;
    };
  })
]
