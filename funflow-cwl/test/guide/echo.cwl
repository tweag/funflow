cwlVersion: v1.0
class: CommandLineTool
id: firsttool
baseCommand: echo
inputs:
  message:
    type: File
    inputBinding:
      position: 1
outputs: []
stdout: out.txt
