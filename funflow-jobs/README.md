funflow-jobs
===

Jobs provide a means to support distribution of complete flows.

Compared with distributing external tasks, distribution of flows is tricky,
since flows contain arbitrary computation. Haskell does not contain a means to 
distribute arbitrary computation, so additional work is needed by the user to 
ensure this is possible.

Currently, the only implementation uses Redis to distribute flows, and relies on
a map of strings to jobs (of a specific shape - e.g. input and output type) which
must be known to all nodes executing the job.
