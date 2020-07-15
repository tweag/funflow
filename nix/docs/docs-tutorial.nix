let
  pkgs = import ./../nixpkgs.nix { };

  project = import ./../default.nix;
in pkgs.runCommand "funflow2-tutorial-generated" {
  buildInputs = [ project.funflow2-tutorial.components.exes.tutorial1 ];
} ''
  mkdir -p $out/docs/tutorial
  cd $out/docs/tutorial
  tutorial1 > tutorial1.html
''
