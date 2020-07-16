let
  pkgs = import ./../nixpkgs.nix { };

  project = import ./../default.nix;
  funflow2-lib-doc = project.funflow.components.library.doc;

  haddockReplaceUrlRegex =
    "s/file:\\/\\/\\/nix\\/store\\/[a-z0-9]\\+.\\+-lib-\\(.\\+\\)-\\([0-9\\.-]\\+\\)-haddock-doc\\/share\\/doc\\/\\(.\\+\\)\\/html\\//https:\\/\\/hackage.haskell.org\\/package\\/\\1-\\2\\/docs\\//g";

in pkgs.runCommand "${funflow2-lib-doc.name}-url-corrected" { } ''
  cp -R ${funflow2-lib-doc}/* $out
  find $out -exec chmod 755 {} \;
  find $out -type f \
      -exec sed -i -e \
          "${haddockReplaceUrlRegex}" \
          {} \;
''
