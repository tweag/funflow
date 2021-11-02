let

  pkgs = import nix/pkgs.nix;

  # Must be the same as the GHC version used by `stack.yaml`!
  ghcVersion = "8107";

  # Wrap Stack to configure Nix integration and target the correct Stack-Nix file.
  # That way non-Nix users are not polluted when using `stack.yaml`.
  # --nix -> Enable Nix support
  # --nix-path=\\"nixpkgs=${pkgs.path}\\" -> Stack uses this nixpkgs definition rather than the system one (defined in `NIX_PATH` environment variable)
  # --nix-shell-file nix/shell-stack.nix -> Specify the Nix file to use (otherwise it uses shell.nix by default, which we don't want)
  stack-wrapped = pkgs.symlinkJoin {
    name = "stack";
    paths = [ pkgs.stack ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/stack \
        --add-flags "\
          --nix \
          --nix-path=\\"nixpkgs=${pkgs.path}\\"
          --nix-shell-file nix/shell-stack.nix \
        "
    '';
  };

in pkgs.mkShell {
  buildInputs = [
    # Packages needed at compilation/runtime #
    pkgs.zlib
    pkgs.docker

    # Development tools #
    # The build tool
    stack-wrapped
    # GHC compiler: can be useful e.g. if one wants to summon `ghci`
    pkgs.haskell.packages."ghc${ghcVersion}".ghc
    # Language Server Protocol (e.g. to use an LSP plugin in VS Code, Atom, Emacs, etc.)
    pkgs.haskell.packages."ghc${ghcVersion}".haskell-language-server
    # Formatter
    pkgs.ormolu

    # Nix integration #
    # Actually depend on Nix itself for pure Nix shells
    pkgs.nix
    # Manage Nix package versions easily
    pkgs.niv
    # Nix file formatter
    pkgs.nixpkgs-fmt
  ];
}
