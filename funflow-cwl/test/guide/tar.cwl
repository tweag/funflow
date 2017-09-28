cwlVersion: v1.0
class: CommandLineTool
baseCommand: [tar, xf]
id: tar-tool
inputs:
  tarfile:
    type: File
    inputBinding:
      position: 1
outputs:
  example_out:
    type: File
    outputBinding:
      glob: hello.txt
