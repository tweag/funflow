# Script for running tutorial notebooks and converting them to html.
{ funflow-tutorial-jupyter, writeScriptBin, stdenv, nbconvert, bash }:
let
  tutorial-notebooks = stdenv.mkDerivation {
    name = "funflow-tutorial-notebooks";
    src = builtins.filterSource (path: type:
      (type != "directory" || baseNameOf path != ".ipynb_checkpoints"))
      ../../funflow-tutorial;
    installPhase = ''
      mkdir $out
      cp -r $src/notebooks $out
    '';
  };
in writeScriptBin "generate-funflow-tutorial" ''
  #!${bash}/bin/bash
  set -e
  set -x
  # Run each notebook and grab its html output.
  [[  "$#" -eq 2 ]] || ( echo "Usage: $0 <path/to/notebooks> <output/directory>" && exit 1)

  ${funflow-tutorial-jupyter.env.shellHook}

  mkdir -p "$2"

  # NOTE: This assumes that all notebooks are intended to be executed using the ihaskell kernel

  # Notebooks in the base directory
  for notebook in "$1/*.ipynb"; do
    echo "$notebook"
    ${nbconvert}/bin/jupyter-nbconvert \
      --ExecutePreprocessor.kernel_name='ihaskell_haskell' \
      --ExecutePreprocessor.timeout=600 \
      --ExecutePreprocessor.allow_errors=False \
      --execute $notebook \
      --output-dir "$2" \
      --to=html
  done

  # Notebooks in their own subdirectory
  for notebook in "$1/**/*.ipynb"; do
    echo "$notebook"
    ${nbconvert}/bin/jupyter-nbconvert \
      --ExecutePreprocessor.kernel_name='ihaskell_haskell' \
      --ExecutePreprocessor.timeout=600 \
      --ExecutePreprocessor.allow_errors=False \
      --execute $notebook \
      --output-dir "$2" \
      --to=html
  done
''
