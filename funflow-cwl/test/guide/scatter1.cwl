#!/usr/bin/env cwl-runner
cwlVersion: v1.0
class: Workflow

inputs:
  inp1: string[]
  inp2: string[]

outputs:
  out:
    outputSource: step1/echo_out
    type: File[]

requirements:
  - class: ScatterFeatureRequirement

steps:
  step1:
    in:
      echo_in1: inp1
      echo_in2: inp2
    out: [echo_out]
    scatter: [echo_in1, echo_in2]
    scatterMethod: flat_crossproduct
    run: echo-pair.cwl

