class: CommandLineTool
cwlVersion: v1.0
baseCommand: ["cat", "example.conf"]
id: createfile
requirements:
  InitialWorkDirRequirement:
    listing:
      - entryname: example.conf
        entry: |
          CONFIGVAR=$(inputs.message)

inputs:
  message: string
outputs: []
