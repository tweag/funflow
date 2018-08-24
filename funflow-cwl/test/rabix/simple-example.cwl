
class: Workflow
cwlVersion: v1.0
id: simple_example
inputs:
  - id: intfunction
    type: File
  - id: sumfiles
    type: File
  - id: evaluator
    type: File
  - id: numFiles
    type: int
  - id: genfiles
    type: File
outputs:
  - id: stdout
    outputSource:
      - eval_sum/stdout
    type: File
steps:
  - id: compile_default_2
    in:
      - id: source
        source: sumfiles
    out:
      - id: binary
    run: ./compile-default.cwl
  - id: eval_sum
    in:
      - id: binarySummer
        source: compile_default_2/binary
      - id: files
        source: compileandexecute/stdoutFile
      - id: evaluator
        source: evaluator
    out:
      - id: stdout
    run: ./eval-sum.cwl
    label: eval-sum
  - id: eval_file_creator
    in:
      - id: numFiles
        source: numFiles
      - id: binaryThatMakesFiles
        source: compile_default/binary
      - id: evaluator
        source: evaluator
    out:
      - id: files
    run: ./eval-file-creator.cwl
    label: eval-file-creator
  - id: compile_default
    in:
      - id: source
        source: genfiles
    out:
      - id: binary
    run: ./compile-default.cwl
    label: compile-default
  - id: compileandexecute
    in:
      - id: source
        source: intfunction
      - id: evaluator
        source: evaluator
      - id: binaryInputFile
        source: eval_file_creator/files
    out:
      - id: stdoutFile
    run: ./compileandexecute.cwl
    label: compileAndExecute
    scatter:
      - binaryInputFile
    scatterMethod: flat_crossproduct
requirements:
  - class: SubworkflowFeatureRequirement
  - class: ScatterFeatureRequirement
