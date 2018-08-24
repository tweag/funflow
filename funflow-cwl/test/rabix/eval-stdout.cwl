class: CommandLineTool
cwlVersion: v1.0
$namespaces:
  sbg: 'https://www.sevenbridges.com'
id: _eval_stdout
baseCommand:
  - bash
inputs:
  - id: binary
    type: File
    inputBinding:
      position: 1
  - id: binaryInputFile
    type: File
    inputBinding:
      position: 2
  - id: evaluator
    type: File
    inputBinding:
      position: 0
outputs:
  - id: stdoutFile
    type: File
    outputBinding:
      glob: stdout.txt
label: eval
stdout: stdout.txt
