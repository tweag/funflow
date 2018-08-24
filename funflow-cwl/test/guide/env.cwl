cwlVersion: v1.0
class: CommandLineTool
id: env
baseCommand: env
requirements:
  - class: EnvVarRequirement
    envDef:
      HELLO: $(inputs.message)
inputs:
  message: string
outputs:
  example_out:
    type: File
    outputBinding:
      glob: output.txt
stdout: output.txt

