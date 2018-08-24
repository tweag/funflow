cwlVersion: v1.0
class: Workflow
id: nestedwf
inputs: []

outputs:
  classout:
    type: File
    outputSource: compile/classout

requirements:
  - class: SubworkflowFeatureRequirement

steps:
  compile:
    run: 1st-workflow.cwl
    in:
      inp:
        source: create-tar/tar
      ex:
        default: "Hello2.java"
    out: [classout]

  create-tar:
    requirements:
      - class: InitialWorkDirRequirement
        listing:
          - entryname: Hello2.java
            entry: |
              public class Hello {
                public static void main(String[] argv) {
                    System.out.println("Hello from Java");
                }
              }
    in: []
    out: [tar]
    run:
      class: CommandLineTool
      requirements:
        - class: ShellCommandRequirement
      arguments:
        - shellQuote: false
          valueFrom: |
            date
            tar cf hello4.tar Hello2.java
            date
      inputs: []
      outputs:
        tar:
          type: File
          outputBinding:
            glob: "hello4.tar"
