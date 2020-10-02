# Wrapper script for calling tutorial executables and bundling their html outputs
{ runCommand
, funflow-tutorial-jupyter
}:
runCommand "generate-funflow-tutorial"
{
  src = ../../funflow-tutorial;
  buildInputs = funflow-tutorial-jupyter.env.buildInputs;
} (funflow-tutorial-jupyter.env.shellHook +
  ''
  set -e
  export HOME=$TMP/tutorial-jupyter
  mkdir $HOME
  mkdir -p $out/share/tutorial
  
  # Run each notebook and grab its html output.
  
  # NOTE: This assumes that all notebooks are intended to be executed using the ihaskell kernel

  # Notebooks in the base directory
  for notebook in $src/notebooks/*.ipynb; do
    echo "$notebook"
    jupyter-nbconvert --ExecutePreprocessor.kernel_name='ihaskell_haskell'  --execute $notebook --output-dir "$out/share/tutorial"
  done

  # Notebooks in their own subdirectory
  for notebook in $src/notebooks/**/*.ipynb; do
    echo "$notebook"
    jupyter-nbconvert --ExecutePreprocessor.kernel_name='ihaskell_haskell'  --execute $notebook --output-dir "$out/share/tutorial"
  done
''
)
