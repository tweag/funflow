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

- FunFlow allows for **seamless composition** of multiple types of computation:

  - *Pure functions* (of type `a -> b`)
  - *IO actions* (of type `a -> IO b`)
  - *External* computations. External computations are executed outside of
    FunFlow. They allow FunFlow to invoke computations from any
    language.
  - *User-defined effects*. FunFlow is extensible with user-defined effects. By
    specifying different interpreters for such effects, you can easily test
    FunFlow in a mock environment.

- FunFlow is designed to be **integrated** into your application. Flows can be
  executed inside your Haskell program, even where they involve external
  computations which are run by other processes. FunFlow's support for
  user-defined effects lets you extend the grammar of a workflow with your own
  domain specific applications.
- FunFlow provides very powerful **caching** for steps.

  - FunFlow's caching is based around the *content store*. This stores all
    artifacts according to the hash of the inputs and computations which
    produced them. FunFlow uses this to know when exactly a computation must be
    rerun and when previous results can be re-used.[2]_
  - The content store also acts as a CAS_ system. This means that, if multiple
    inputs produce the same output, that output will be stored only once on disk
    (and subsequent computations will not be rerun).

    This is particularly useful for build systems, where if you change your
    input in such a way that the ouptut is not altered, there is no need to run
    the rest of the computation again.
  - Content store caching is used by default for all external steps, and can be
    enabled for internal computations by providing suitable
    serialisation/deserialisation functions.

- **Failure handling**. In a long-running workflow, failures are inevitable. FunFlow
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

- Support for **distributing complete workflows**, rather than just individual
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

Here's an example of a simple workflow, defined from basic Haskell functions::

  -- | A flow which doubles each element of a list, and then reverses the list
  doubleAndReverse :: SimpleFlow [Int] [Int]
  doubleAndReverse = mapA (step (*2)) >>> step reverse

`step`
  The `step` function takes a Haskell function of type `a -> b` and lifts it into
  a `SimpleFlow a b`. [1]_
`mapA`
  The `mapA` combinator applies a flow over a list.
`>>>`
  `>>>` is an `arrow combinator
  <http://hackage.haskell.org/package/base-4.10.1.0/docs/Control-Category.html#v:-62--62--62->`_
  which composes two flows sequentially.

You can also use Haskell's arrow notation to compose more complex flows::

  -- | Flow which asks for the user's name, then their favourite number.
  --   The favourite number will be cached, such that if the user comes back,
  --   they will not be asked again.
  favouriteNumber = proc () -> do
    name <- stepIO (\_ -> putStrLn "Hi, what's your name?" >> const getLine) -< ()
    fnum <- stepIO' (def { cache = defaultCacherWithIdent 172652 })
                    (\_ -> putStrLn "What's your favourite number?" >> const getLine) -< name
    returnA -< (name, fnum)

In this flow, you can also see the use of FunFlow's caching.

`stepIO`
  `stepIO` lifts an `IO` action into a flow.
`stepIO'`
  `stepIO'` is a more complex variant of `stepIO` that allow for configuring things
  such as the caching properties.

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

  -- | Flow which asks for the user's name, then their favourite number.
  --   The favourite number will be cached, such that if the user comes back,
  --   they will not be asked again.
  favouriteNumber :: SimpleFlow () (String, String)
  favouriteNumber = proc () -> do
    name <- stepIO (\_ -> putStrLn "Hi, what's your name?" >> getLine) -< ()
    -- We enable caching for this step. The default cacher uses 'Store' instances
    -- to provide serialisation/deserialisation. The ident is used to ensure that
    -- multiple different steps with the same input do not resolve to the same
    -- cache item.
    fnum <- stepIO' (def { cache = defaultCacherWithIdent 172652 })
                    (\_ -> putStrLn "What's your favourite number?" >> getLine) -< name
    returnA -< (name, fnum)

  -- | Runs the 'favourite number' flow in IO.
  runFavouriteNumber :: IO ()
  runFavouriteNumber = do
    -- Use /tmp/funflow as the path to our content store.
    res <- withSimpleLocalRunner [absdir|/tmp/funflow|] $ \run ->
      -- 'run' takes the flow and the initial input. Since this flow has an input type of '()',
      -- this is what we provide.
      run favouriteNumber ()
    case res of
      Left err -> putStrLn $ "Something went wrong: " ++ show err
      Right (name, num) -> putStrLn $ "Hi, " ++ name ++ ", your favourite number is " ++ num

Defining external tasks
-----------------------

Use external tasks to run steps outside of the Haskell process. Fundamentally,
an external task will resolve to a command-line call to another application.
While this could be done with `stepIO`, but using external tasks has a few
big advantages:

- External tasks fit in naturally with the content store framework. Both the
  inputs to the task and the actual definition of the task are used to
  determine the resultant hash, so that if, say, a script changes, the
  results will be recomputed. This is hard to achieve with `stepIO`.
- External tasks are naturally distributable. When running in a production setting,
  you're likely to want to distribute tasks among multiple machines. This is
  not, in general, achievable with `IO` computations.
- `IO` steps are opaque to inspection. External tasks, on the other hand, can be
  visualised in the workflow graph, and their stdout/stderr streams are captured
  automatically.

FunFlow's current approach to external tasks is heavily based on Docker_. Using
Docker allows tasks to be self-contained, and adds minimal requirements to the
system being used to host FunFlow instances (they just need to have docker
running).

To use a docker container as an external step, define a function of type `a ->
Docker.Config`, where `a` is the input type to the flow. At its core, this
means:

- Specifying the docker image (and optionally, image ID) to use. For example,
  `nixos/nix:1.11.14`.
- Specifying the path to the command which will be run within the container.
- Specifying which inputs (from the content store) are to be mounted within
  the container.

Here's an example::

  myDockerStep :: SimpleFlow CS.Item CS.Item
  myDockerStep = docker $ \input -> Docker.Config
    { -- Set the docker image to use for this step
      Docker.image = "nixos/nix"
      -- Optionally, you can define a specific tag to use, to fix the version.
    , Docker.optImageID = Just "1.11.14"
      -- Define how the inputs are mounted into the container. We can either
      -- have a single input, which will be mounted at /input/, or multiple
      -- inputs, which will be mounted as subdirectories inside /input/.
    , Docker.input = Docker.SingleInput input
      -- Command to run inside the container. It's best to use an absolute
      -- path here.
    , Docker.command = "/root/myScript.sh"
      -- Additional arguments to pass to the script being run.
    , Docker.args =
        [
        , "--input_dir", "/input/"
        , "--output_dir", "/output/"
        ]
    }

A `CS.Item` refers to an item within the content store. You can use
`putInStore`, `getFromStore`, `copyFileToStore` and similar tools to add and
fetch files from the store. This lets you interleave internal and external
computations. Here's an example of a more complex flow using both internal
and external computation::

  -- | This flow takes a string which is assumed to be the source code
  --   for a 'C' function. It writes this to a file, then uses two external
  --   steps to compile and run the function. The resulting 'stdout' is read
  --   in and presented to the user.
  compileAndRunC :: SimpleFlow String String
  compileAndRunC = proc csrc -> do
      cInput <- writeString -< (csrc, [relfile|out.c|])
      scriptInput <- writeExecutableString -< (compileScript, [relfile|compile.sh|])
      compiled <- compileDocker -< (cInput, scriptInput)
      result <- runDocker -< compiled
      readString_ -< result
    where
      compileScript =
        " #!/usr/bin/env nix-shell \n\
        \ #! nix-shell -i bash -p gcc \n\
        \ gcc -o $2 $1 "

      compileDocker = docker $ \(cInput, scriptInput) -> Docker.Config
        { Docker.image = "nixos/nix"
        , Docker.optImageID = Just "1.11.14"
        , Docker.input = Docker.MultiInput
          $ Map.fromList [ ("script", CS.contentItem scriptInput)
                        , ("data", CS.contentItem cInput)
                        ]
        , Docker.command = "/input/script/compile.sh"
        , Docker.args = ["/input/data/out.c", "/output/out"]
        }
      runDocker = docker $ \input -> Docker.Config
        { Docker.image = "nixos/nix"
        , Docker.optImageID = Just "1.11.14"
        , Docker.input = Docker.SingleInput input
        , Docker.command = "bash -c"
        , Docker.args = ["\"/input/out > /output/out\""]
        }

Running on multiple machines
----------------------------

To run on multiple machines, you need to use one of the distributable
coordinators - either the Redis coordinator or the SQLite coordinator. To do
this, you need to:

1. Start some *executors* pointed at the coordinator. An executor is a process which
   reads tasks from the coordinator and executes them.
2. Run your flow using that coordinator.

The simplest way to run an executor is to use the bundled `ffexecutord` executable.
This can work with either the Redis or SQLite coordinators.

Here's an example of initialising an executor using `/tmp/funflow` as the content
store directory, and `/tmp/coordinator.db` as our coordinating database::

  ffexecutord sqlite /tmp/funflow /tmp/coordinator.db

You then need to run the flow, pointing at this coordinator. To do so, you'll need a
slightly more complex function from Control.FunFlow.Exec.Simple_: `runSimpleFlow`. You
need to give this the correct parameters for the `SQLite` coordinator::

  CS.withStore [absdir|/tmp/funflow|] $ \store -> do
    runFlow SQLite [absfile|/tmp/coordinator.db|] store runNoEffect 123123 flow input

A couple of the parameters here may be confusing:

runNoEffect
  This is used to handle any user-defined effects in the flow. Since
  there are none here, you can use `runNoEffect`.

123123
  This is a random integer used in helping to determine the hashes for caching
  internal steps. It's needed because there might be parts of the environment
  which FunFlow is unaware of but which have an impact on the results of
  computations, and so should form part of the cache.

.. [1] Technically, it lifts it to the more general type `Flow eff ex a b`, but
       that full generality is not needed here.
.. [2] This is heavily inspired by the nix_ package manager.
.. _nix: https://nixos.org/nix
.. _CAS: https://en.wikipedia.org/wiki/Content-addressable_storage
.. _arrows: https://www.haskell.org/arrows/
.. _Docker: https://www.docker.com
.. _Control.FunFlow.Exec.Simple: ./funflow/src/Control/FunFlow/Exec/Simple.hs
