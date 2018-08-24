cwlVersion: v1.0
class: CommandLineTool
id: arguments
label: Example trivial wrapper for Java 7 compiler
requirements:
  - class: DockerRequirement
    dockerPull: java:7-jdk
baseCommand: javac
arguments: ["-d", "."]
inputs:
  src:
    type: File
    inputBinding:
      position: 1
outputs:
  classfile:
    type: File
    outputBinding:
      glob: "*.class"
