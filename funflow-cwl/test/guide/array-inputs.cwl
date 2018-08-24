cwlVersion: v1.0
class: CommandLineTool
id: arrinps
baseCommand: echo
inputs:
  filesA:
    type: string[]
    inputBinding:
      prefix: -A
      position: 1

  filesB:
    type: string[]
    inputBinding:
      position: 2

  filesC:
    type: string[]
    inputBinding:
      prefix: -C=
      itemSeparator: ","
      separate: false
      position: 4

outputs:
  out:
    type: File
    outputBinding:
      glob: output.txt

stdout: output.txt

