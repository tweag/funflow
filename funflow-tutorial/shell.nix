let
  pkgs = import ../nix/pkgs.nix;

  # The top level `shell.nix` we want to extend with Jupyter integration
  rootShellNix = import ../shell.nix;

  # A Jupyter shell integration provided by `overlays.nix`
  jupyterEnv = pkgs.funflow-tutorial-jupyter.env;

  # Inherit the root `shell.nix` and extend it with the Jupyter environment
in rootShellNix.overrideAttrs (old: {
  buildInputs = old.buildInputs ++ jupyterEnv.buildInputs;
  nativeBuildInputs = old.nativeBuildInputs ++ jupyterEnv.nativeBuildInputs;
  shellHook = old.shellHook + jupyterEnv.shellHook;
})
