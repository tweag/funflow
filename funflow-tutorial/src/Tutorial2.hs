{-# OPTIONS_GHC -F -pgmF inlitpp #-}

# Developer Guide [WIP]

This tutorial aims to help prospective funflow developers get started with creating new tasks and functions 
for executing those tasks (i.e. interpreters). 

## Defining Tasks 

Tasks are defined in `Funflow.Tasks` and are simply just datatypes. For Tasks to be incorporated into a `Flow` DAG, they must specify an
instance `IsFlow` for your new `Task` datatype. See `Funflow.Flow` for examples. 

TODO - Go into more detail and describe why `strand` is used in IsFlow

## Compile, Load, and Execution Time 

TODO

## Tasks, Effects and Interpretation

TODO

Funflow `Tasks` describe a specific type of computation which is then _interpreted_ at load time. Another way to put it is that 
`Tasks` can be seen as _computational effects_. This means that we can separately specify __how__ a Task description is transformed 
into executable code. Since `Tasks` are tagged by what they do, the `Flow` type can assemble all effects or constrain them to specific
subsets at load time which is useful to achieve highly reproducible workflows. 
