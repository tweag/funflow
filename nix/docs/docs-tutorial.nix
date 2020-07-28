let
  pkgs = import ./../nixpkgs.nix { };

  project = import ./../default.nix;
in pkgs.runCommand "funflow-tutorial-generated" {
  src = ../../funflow-tutorial;
  buildInputs = [
    project.funflow-tutorial.components.exes.quick-reference
    project.funflow-tutorial.components.exes.tutorial1
    project.funflow-tutorial.components.exes.wordcount
  ];
  # wordcount reads a "words.txt" file from the working directory
  # Here, we take the example included with funflow-tutorial
} ''
  mkdir -p $out/docs/tutorial
  cp $src/words.txt .
  quick-reference > $out/docs/tutorial/quick-reference.html
  tutorial1 > $out/docs/tutorial/tutorial1.html
  wordcount > $out/docs/tutorial/wordcount.html
''
