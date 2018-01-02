FunFlow
=======

.. highlight:: haskell
.. default-role:: code

.. image:: https://circleci.com/gh/tweag/funflow.svg?style=svg
    :target: https://circleci.com/gh/tweag/funflow

FunFlow is a library and tools to compose and run computational workflows. 
A workflow is a computation built up of multiple steps. These steps are then
wired together into a larger computation. Outputs from previous steps can form
inputs to subsequent steps. 

Features
--------

- Funflow is designed to be **integrated** into your application. It allows for
  seamless composition of multiple types of computation:

  - Pure functions (of type `a -> b`)
  - IO actions (of type `a -> IO b`)
  - *External* computations. External computations are executed outside of the
    Haskell process. They allow funflow to invoke computations from any
    language.
  - User-defined actions. FunFlow is extensible with user-defined actions. By
    specifying different interpreters for such actions, you can easily test
    funflow in a mock environment.

- Funflow provides very powerful **caching** for steps.

  - FunFlow's caching is based around the *content store*. Inspired by the nix_
    package manager, this links all artifacts according to the hash of the
    inputs and computations which produced them. FunFlow uses this to know when
    exactly a computation must be rerun and when previous results can be re-used.
  - The content store also acts as a CAS_ system. This means that, if multiple
    inputs produce the same output, that output will be stored only once on disk
    (and subsequent computations will not be rerun). This is particularly useful
    for build systems: if you change your input in such a way that the ouptut
    is not altered, there is no need to run the rest of the computation again.
  - Content store caching is used by default for all external steps, and can be
    enabled for internal computations by providing suitable
    serialisation/deserialisation functions.

- **Failure handling**. In a long running workflow, failures are inevitable. FunFlow
  has support for handling failures inside workflows, and for resuming workflows
  from the last successful point once some external error has been corrected.
- Sequential and **parallel execution**. FunFlow executes sections of workflows
  in parallel where this is possible, and handles sequential execution where
  tasks have dependencies.
- **Task distribution**. External steps can be serialised and run remotely. FunFlow
  includes a number of central coordinators which handle distributing steps
  among multiple machines:

  - An `in-memory <./funflow/src/Control/FunFlow/External/Coordinator/Memory.hs>`_
    coordinator, useful for single-process computation.
  - A `Redis based <./funflow/src/Control/FunFlow/External/Coordinator/Redis.hs>`_
    coordinator.
  - A `SQLite <./funflow/src/Control/FunFlow/External/Coordinator/SQLite.hs>`_
    coordinator, which uses a SQLite database as a shared location for task
    distribution.

- Support for distributing complete workflows, rather than just individual
  steps. The `funflow-jobs <./funflow-jobs>`_ package provides support for
  distributing full workflows using Redis. This has some extra constraints
  compared to distributing external tasks, since Haskell functions are in
  general not serialisable.
- Safe and powerful workflow composition. FunFlow takes advantage of Haskell's
  type system to ensure flows are composed in a safe manner. And since the
  composition of flows results in a flow, it's easy to include one complex
  workflow as part of another.

Defining a workflow
-------------------

A simple workflow can be defined from basic Haskell functions::

  -- | A flow which doubles and reverses a list
  doubleAndReverse :: SimpleFlow [Int] [Int]
  doubleAndReverse = mapA (step (*2)) >>> step reverse

The `step` function takes a Haskell function of type `a -> b` and lifts it into
a `SimpleFlow a b`. [1]_ The `mapA` combinator applies a flow over a list, and
`>>>` composes two flows sequentially.

We can also use Haskell's arrow notation to compose more complex flows::

  -- | Flow which asks for the user's name, then their favourite number.
  --   The favourite number will be cached, such that if the user comes back,
  --   they will not be asked again.
  favouriteNumber = proc () -> do
    name <- stepIO (\_ -> putStrLn "Hi, what's your name?" >> const getLine) -< ()
    fnum <- stepIO' (def { cache = defaultCacherWithIdent 172652 })
                    (\_ -> putStrLn "What's your favourite number?" >> const getLine) -< name
    returnA -< (name, fnum)

In this flow, we can also see the use of FunFlow's caching. `stepIO` lets us lift
an `IO` action into a flow. `stepIO'` is a more complex variant that lets us configure
things such as the caching properties.

Running a workflow
------------------

To run a workflow, use one of the options from Control.FunFlow.Exec.Simple_ .
The easiest place to start is with `withSimpleLocalRunner`. This takes the
absolute path to a directory used to host the content store. It returns a
function which can be used to run your workflow.

Here's an example of a complete pipeline, along with a runner::

  {-# LANGUAGE Arrows #-}
  {-# LANGUAGE QuasiQuotes #-}
  import Control.Arrow
  import Control.FunFlow
  import Control.FunFlow.Exec.Simple (withSimpleLocalRunner)
  import Data.Default
  import Path

  favouriteNumber :: SimpleFlow () (String, String)
  favouriteNumber = proc () -> do
    name <- stepIO (\_ -> putStrLn "Hi, what's your name?" >> getLine) -< ()
    fnum <- stepIO' (def { cache = defaultCacherWithIdent 172652 })
                    (\_ -> putStrLn "What's your favourite number?" >> getLine) -< name
    returnA -< (name, fnum)

  runFavouriteNumber :: IO ()
  runFavouriteNumber = do
    res <- withSimpleLocalRunner [absdir|/tmp/funflow|] $ \run ->
      run favouriteNumber ()
    case res of
      Left err -> putStrLn $ "Something went wrong: " ++ show err
      Right (name, num) -> putStrLn $ "Hi, " ++ name ++ ", your favourite number is " ++ num

Defining external tasks
-----------------------

External tasks are used to run steps outside of the Haskell process.
Fundamentally, an external task will resolve to a command-line call to another
application. This can obviously be done with `stepIO`, but using external tasks
has a couple of big advantages:

- External tasks fit in naturally with the content store framework. Both the
  inputs to the task and the actual definition of the task will be used to
  determine the resultant hash, so that if, say, a script changes, the
  results will be recomputed. This is hard to achieve with `stepIO`.
- External tasks are naturally distributable. When running in a production setting,
  you're likely to want to distribute tasks among multiple machines. This is
  not, in general, achievable with arbitrary `IO` computations.

FunFlow's current approach to external tasks is heavily based on Docker_. Using
Docker allows tasks to be self-contained, and adds minimal requirements to the
system being used to host funflow instances (they just need to have docker running!)

To use a docker container as an external step, one simply needs to define a function
of type `a -> Docker.Config`, where `a` is the input type to the flow. At its core,
this means:

- Specifying the docker image (and optionally, image ID) to use. For example,
  `nixos/nix:1.11.14`.
- Specifying the path to the command which will be run within the container.
- Specifying which inputs (from the content store) are to be mounted within
  the container.

Here's an example::

  myDockerStep :: SimpleFlow CS.Item CS.Item
  myDockerStep = docker $ \input -> Docker.Config
    { Docker.image = "nixos/nix"
    , Docker.optImageID = Just "1.11.14"
    , Docker.input = Docker.SingleInput input
    , Docker.command = "myScript.sh"
    , Docker.args =
        [
        , "--input_dir", "/input/"
        , "--output_dir", "/output/"
        ]
    }

A `CS.Item` refers to an item within the content store. You can use
`putInStore`, `getFromStore`, `copyFileToStore` and similar tools to add and
fetch files from the store. This lets you interleave internal and external
computations.

Running on multiple machines
----------------------------

To run on multiple machines, you need to use one of the distributable
coordinators - either the Redis coordinator or the SQLite coordinator. To do
this, you need to:

- Start some *executors* pointed at the coordinator.
- Run your flow using that coordinator.

The simplest way to run an executor is to use the bundled `ffexecutord` executable.
This can work with either the Redis or SQLite coordinators. Here's how we might
initialise an executor using `/tmp/funflow` as the content store directory, and
`/tmp/coordinator.db` as our coordinating database::

  ffexecutord sqlite /tmp/funflow /tmp/coordinator.db

We then need to run the flow, pointing at this coordinator. To do so, we'll need a
slightly more complex function from Control.FunFlow.Exec.Simple_: `runSimpleFlow`. We
need to give this the correct parameters for the `SQLite` coordinator::

  CS.withStore [absdir|/tmp/funflow|] $ \store -> do
    runFlow SQLite [absfile|/tmp/coordinator.db|] store runNoEffect 123123 flow input

A couple of the parameters here may be confusing:

runNoEffect
  This is used to handle any user-defined effects in the flow. Since
  we have none here, we can use `runNoEffect`.

123123
  This is a random integer used in helping to determine the hashes for caching
  internal steps. It's needed because there might be parts of the environment
  which FunFlow is unaware of but which have an impact on the results of
  computations, and so should form part of the cache.


.. [1] Technically, it lifts it to the more general type `Flow eff ex a b`, but
       that full generality is not needed here.
.. _nix: https://nixos.org/nix
.. _CAS: https://en.wikipedia.org/wiki/Content-addressable_storage
.. _arrows: https://www.haskell.org/arrows/
.. _Docker: https://www.docker.com
.. _Control.FunFlow.Exec.Simple: ./funflow/src/Control/FunFlow/Exec/Simple.hs
