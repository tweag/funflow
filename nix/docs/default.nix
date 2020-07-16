let
  pkgs = import ../nixpkgs.nix { };

  docs-api = import ./docs-api.nix;
  docs-tutorial = import ./docs-tutorial.nix;
in pkgs.runCommand "funflow-docs" { } ''
  mkdir -p $out/docs/api
  cp -R ${docs-api}/doc/funflow/html/* $out/docs/api
  mkdir -p $out/docs/tutorial
  cp -R ${docs-tutorial}/docs/tutorial/* $out/docs/tutorial
''
