class: CommandLineTool
cwlVersion: v1.0
$namespaces:
  sbg: 'https://www.sevenbridges.com'
id: argtest
baseCommand:
  - echo
inputs:
  - id: instr
    type: string
    inputBinding:
      position: 0
outputs:
  - id: output
    type: File
    outputBinding:
      glob: out.txt
label: paramtest
arguments:
  - position: -1
    prefix: ''
    valueFrom: arg1
stdout: out.txt
