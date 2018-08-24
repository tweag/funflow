class: CommandLineTool
cwlVersion: v1.0
$namespaces:
  sbg: 'https://www.sevenbridges.com'
id: compile_default
baseCommand:
  - gcc
inputs:
  - id: source
    type: File
    inputBinding:
      position: 0
outputs:
  - id: binary
    type: File
    outputBinding:
      glob: a.out
label: compile-default
requirements:
  - class: DockerRequirement
    dockerPull: gcc
