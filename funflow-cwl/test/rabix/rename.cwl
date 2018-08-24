class: CommandLineTool
cwlVersion: v1.0
$namespaces:
  sbg: 'https://www.sevenbridges.com'
id: rename
baseCommand:
  - cp
inputs:
  - id: newName
    type: string
    inputBinding:
      position: 1
  - id: toRename
    type: File
    inputBinding:
      position: 0
outputs:
  - id: output
    type: File
    outputBinding:
      glob: $(inputs.newName)
label: rename
requirements:
  - class: InlineJavascriptRequirement
