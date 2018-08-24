class: Workflow
cwlVersion: v1.0
id: test
label: simple-wf
$namespaces:
  sbg: 'https://www.sevenbridges.com'
inputs:
  - id: outName
    type: string
    'sbg:x': -731
    'sbg:y': -342
  - id: str
    type: string
    'sbg:x': -726
    'sbg:y': -175
  - id: newName
    type: string
    'sbg:x': -721
    'sbg:y': 16
outputs:
  - id: executable
    outputSource:
      - compile/executable
    type: File
    'sbg:x': 263
    'sbg:y': -100
steps:
  - id: compile
    in:
      - id: mainfile
        source: rename/output
      - id: outName
        source: outName
    out:
      - id: executable
    run: ./compile.cwl
    label: test-syntax
    'sbg:x': 51
    'sbg:y': -98
  - id: rename
    in:
      - id: newName
        source: newName
      - id: toRename
        source: write_file/echoed
    out:
      - id: output
    run: ./rename.cwl
    label: rename
    'sbg:x': -275
    'sbg:y': -107
  - id: write_file
    in:
      - id: str
        source: str
    out:
      - id: echoed
    run: ./write-file.cwl
    label: echo-things
    'sbg:x': -494
    'sbg:y': -145
requirements: []
