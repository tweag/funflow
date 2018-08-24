class: CommandLineTool
cwlVersion: v1.0
$namespaces:
  sbg: 'https://www.sevenbridges.com'
id: touchfile
baseCommand:
  - touch
inputs:
  - id: name
    type: string
    inputBinding:
      position: 0
outputs:
  - id: touched
    type: File
    outputBinding:
      glob: $(inputs.name)
label: touchFile
requirements:
  - class: InlineJavascriptRequirement
