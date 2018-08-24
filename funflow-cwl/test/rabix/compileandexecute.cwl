class: Workflow
cwlVersion: v1.0
id: compileandexecute
label: compileAndExecute
$namespaces:
  sbg: 'https://www.sevenbridges.com'
inputs:
  - id: source
    type: File
    'sbg:x': -616
    'sbg:y': 23
  - id: evaluator
    type: File
    'sbg:x': -623
    'sbg:y': -293
  - id: binaryInputFile
    type: File
    'sbg:x': -619
    'sbg:y': -146
outputs:
  - id: stdoutFile
    outputSource:
      - _eval_stdout/stdoutFile
    type: File
    'sbg:x': 222.60113525390625
    'sbg:y': -158.5
steps:
  - id: compile_default
    in:
      - id: source
        source: source
    out:
      - id: binary
    run: ./compile-default.cwl
    label: compile-default
    'sbg:x': -343.3984375
    'sbg:y': -35.5
  - id: _eval_stdout
    in:
      - id: binary
        source: compile_default/binary
      - id: binaryInputFile
        source: binaryInputFile
      - id: evaluator
        source: evaluator
    out:
      - id: stdoutFile
    run: ./eval-stdout.cwl
    label: eval
    'sbg:x': -109
    'sbg:y': -170
requirements: []
