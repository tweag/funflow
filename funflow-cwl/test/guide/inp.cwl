cwlVersion: v1.0
class: CommandLineTool
baseCommand: echo
id: inp
inputs:
  example_flag:
    type: boolean
    inputBinding:
      position: 1
      prefix: -f
  example_string:
    type: string
    inputBinding:
      position: 3
      prefix: --example-string
  example_int:
    type: int
    inputBinding:
      position: 2
      prefix: -i
      separate: false
  example_file:
    type: File?
    inputBinding:
      prefix: --file=
      separate: false
      position: 4

outputs:
   out:
     type: File
     outputBinding:
       glob: out.txt

stdout: out.txt
