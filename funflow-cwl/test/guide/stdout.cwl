cwlVersion: v1.0
class: CommandLineTool
baseCommand: echo
id: stdout
stdout: output.txt
inputs:
  message:
    type: string
    inputBinding:
      position: 1
outputs:
  output:
    type: File
    outputBinding:
      glob: output.txt
