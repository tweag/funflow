class: CommandLineTool
cwlVersion: v1.0
$namespaces:
  sbg: 'https://www.sevenbridges.com'
id: write_file
baseCommand:
  - printf
inputs:
  - id: str
    type: string
    inputBinding:
      position: 0
outputs:
  - id: echoed
    type: File
    outputBinding:
      glob: writtenFile
label: echo-things
stdout: writtenFile
