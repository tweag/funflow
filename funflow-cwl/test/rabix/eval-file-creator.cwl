class: CommandLineTool
cwlVersion: v1.0
$namespaces:
  sbg: 'https://www.sevenbridges.com'
id: eval_file_creator
baseCommand:
  - bash
inputs:
  - id: numFiles
    type: int
    inputBinding:
      position: 2
  - id: binaryThatMakesFiles
    type: File
    inputBinding:
      position: 1
  - id: evaluator
    type: File
    inputBinding:
      position: 0
outputs:
  - id: files
    type: 'File[]'
    outputBinding:
      glob: file*
label: eval-file-creator
