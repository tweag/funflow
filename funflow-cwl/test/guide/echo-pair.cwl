cwlVersion: v1.0
class: CommandLineTool
id: step1command
inputs:
  echo_in1:
    type: string
    inputBinding:
      position: 1
  echo_in2:
    type: string
    inputBinding:
      position: 2
outputs:
  echo_out:
    type: File
    outputBinding:
      glob: "step1_out"
baseCommand: "echo"
arguments:
  - "-n"
  - "foo"
stdout: step1_out
