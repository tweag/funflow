class: CommandLineTool
cwlVersion: v1.0
$namespaces:
  sbg: 'https://www.sevenbridges.com'
id: compdocker
baseCommand:
  - gcc
inputs:
  - id: file
    type: File
    inputBinding:
      position: 0
  - id: execname
    type: string
    inputBinding:
      position: 1
      prefix: '-o'
outputs:
  - id: output
    type: File
    outputBinding:
      glob: $(inputs.execname)
label: compDocker
requirements:
  - class: DockerRequirement
    dockerPull: gcc
  - class: InlineJavascriptRequirement
