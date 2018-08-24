class: CommandLineTool
cwlVersion: v1.0
$namespaces:
  sbg: 'https://www.sevenbridges.com'
id: eval_sum
baseCommand:
  - bash
inputs:
  - id: binarySummer
    type: File
    inputBinding:
      position: 1
  - id: files
    type: 'File[]'
    inputBinding:
      position: 2
  - id: evaluator
    type: File
    inputBinding:
      position: 0
outputs:
  - id: stdout
    type: File
    outputBinding:
      glob: out.txt
label: eval-sum
stdout: out.txt
