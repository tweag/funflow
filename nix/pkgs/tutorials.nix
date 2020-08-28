# Wrapper script for calling tutorial executables and bundling their html outputs
{ runCommand
, funflow-tutorial
}:
runCommand "generate-funflow-tutorial"
{
  src = ../../funflow-tutorial;
  buildInputs = [
    funflow-tutorial.quick-reference
    funflow-tutorial.tutorial1
    funflow-tutorial.wordcount
  ];
  # wordcount reads a "words.txt" file from the working directory
  # Here, we take the example included with funflow-tutorial
} ''
  mkdir -p $out/share/tutorial
  cp $src/words.txt .
  quick-reference > $out/share/tutorial/quick-reference.html
  tutorial1 > $out/share/tutorial/tutorial1.html
  wordcount > $out/share/tutorial/wordcount.html
''
