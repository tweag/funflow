class: CommandLineTool
cwlVersion: v1.0
$namespaces:
  sbg: 'https://www.sevenbridges.com'
id: compile
baseCommand:
  - gcc
inputs:
  - id: mainfile
    type: File
    inputBinding:
      position: 0
  - id: outName
    type: string
    inputBinding:
      position: 1
      prefix: '-o'
outputs:
  - id: executable
    type: File
    outputBinding:
      glob: $(inputs.outName)
label: test-syntax
stdout: out.txt
requirements:
  - class: InlineJavascriptRequirement
