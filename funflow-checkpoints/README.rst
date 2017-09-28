Funflow-checkpoints
======

.. highlight:: haskell
.. default-role:: code

The `funflow-checkpoints` package adds support for adding 'checkpoints' to
Funflow workflows. A checkpoint is a point in the flow at which we can choose to
stop the execution. Using checkpoints provides an alternative to defining
multiple workflows where you may want to run only part of a flow.

Checkpoints operate by means of the additional `ArrowFlow` type. This supports
all the additional flow operations, as well as a `checkpoint` effect. The
various `extractTo` functions can be used to extract a regular flow up to a
given checkpoint for subsequent execution.
