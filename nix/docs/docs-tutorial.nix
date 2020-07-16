let
  pkgs = import ./../nixpkgs.nix { };

  project = import ./../default.nix;
in pkgs.runCommand "funflow-tutorial-generated" {
  buildInputs = [
    project.funflow-tutorial.components.exes.quick-reference
    project.funflow-tutorial.components.exes.tutorial1
  ];
} ''
  mkdir -p $out/docs/tutorial
  cd $out/docs/tutorial
  quick-reference > quick-reference.html
  tutorial1 > tutorial1.html
''
