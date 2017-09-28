Funflow
=======

.. highlight:: haskell
.. default-role:: code

.. image:: https://circleci.com/gh/tweag/funflow.svg?style=svg
    :target: https://circleci.com/gh/tweag/funflow

Funflow is a library and tools to compose and run computational workflows.
A workflow is a computation built up of multiple steps. These steps are then
wired together into a larger computation. Outputs from previous steps can form
inputs to subsequent steps.

For more in-depth context, see the Funflow announcement_.

Features
--------

- Funflow allows for **seamless composition** of multiple types of computation:

  - *Pure functions* (of type `a -> b`)
  - *IO actions* (of type `a -> IO b`)
  - *External* computations. External computations are executed outside of
    Funflow. They allow Funflow to invoke computations from any
    language.
  - *User-defined effects*. Funflow is extensible with user-defined effects. By
    specifying different interpreters for such effects, you can easily test
    Funflow in a mock environment.

- Funflow is designed to be **integrated** into your application. Flows can be
  executed inside your Haskell program, even where they involve external
  computations which are run by other processes. Funflow's support for
  user-defined effects lets you extend the grammar of a workflow with your own
  domain specific applications.
- Funflow provides very powerful **caching** for steps.

  - Funflow's caching is based around the *content store*. This stores all
    artifacts according to the hash of the inputs and computations which
    produced them. Funflow uses this to know when exactly a computation must be
    rerun and when previous results can be re-used. [2]_
  - The content store also acts as a CAS_ system. This means that, if multiple
    inputs produce the same output, that output will be stored only once on disk
    (and subsequent computations will not be rerun).

    This is particularly useful for build systems, where if you change your
    input in such a way that the ouptut is not altered, there is no need to run
    the rest of the computation again.
  - Content store caching is used by default for all external steps, and can be
    enabled for internal computations by providing suitable
    serialisation/deserialisation functions.

- **Failure handling**. In a long-running workflow, failures are inevitable. Funflow
  has support for handling failures inside workflows, and for resuming workflows
  from the last successful point once some external error has been corrected.
- Sequential and **parallel execution**. Funflow executes sections of workflows
  in parallel where this is possible, and handles sequential execution where
  tasks have dependencies.
- **Task distribution**. External steps can be serialised and run remotely. Funflow
  includes a number of central coordinators which handle distributing steps
  among multiple machines:

  - An `in-memory <./funflow/src/Control/Funflow/External/Coordinator/Memory.hs>`_
    coordinator, useful for single-process computation.
  - A `Redis based <./funflow/src/Control/Funflow/External/Coordinator/Redis.hs>`_
    coordinator.
  - A `SQLite <./funflow/src/Control/Funflow/External/Coordinator/SQLite.hs>`_
    coordinator, which uses a SQLite database as a shared location for task
    distribution.

- Support for **distributing complete workflows**, rather than just individual
  steps. The `funflow-jobs <./funflow-jobs>`_ package provides support for
  distributing full workflows using Redis. This has some extra constraints
  compared to distributing external tasks, since Haskell functions are in
  general not serialisable.
- Safe and powerful workflow composition. Funflow takes advantage of Haskell's
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

In this flow, you can also see the use of Funflow's caching.

`stepIO`
  `stepIO` lifts an `IO` action into a flow.
`stepIO'`
  `stepIO'` is a more complex variant of `stepIO` that allow for configuring things
  such as the caching properties.

Running a workflow
------------------

To run a workflow, use one of the options from Control.Funflow.Exec.Simple_ .
The easiest place to start is with `withSimpleLocalRunner`. This takes the
absolute path to a directory used to host the content store. It returns a
function which can be used to run your workflow.

Here's an example of a complete pipeline, along with a runner::

  {-# LANGUAGE Arrows #-}
  {-# LANGUAGE QuasiQuotes #-}
  import Control.Arrow
  import Control.Funflow
  import Control.Funflow.Exec.Simple (withSimpleLocalRunner)
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

Funflow's current approach to external tasks is heavily based on Docker_. Using
Docker allows tasks to be self-contained, and adds minimal requirements to the
system being used to host Funflow instances (they just need to have docker
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
slightly more complex function from Control.Funflow.Exec.Simple_: `runSimpleFlow`. You
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
  which Funflow is unaware of but which have an impact on the results of
  computations, and so should form part of the cache.

User-defined effects
--------------------

Funflow allows you to extend the possible steps in a flow with your own
user-defined effects. Suppose for example you are working on a flow which talks
to a REST service offering details of your record collection. Then you might
define the following grammar for interacting with it::

  -- | Example grammar for dealing with your record collection.
  data RecordCollectionAction a b where
    Insert :: DatabaseAction Record ()
    Select :: DatabaseAction Ix (Maybe Record)
    Delete :: DatabaseAction Ix ()

As with external actions, you will note that this is all possible using `stepIO`. But
as with external actions, there are some benefits to defining your own effects:

- By using effects, you can choose whether details need to be provided at
  workflow construction or execution time. In the above example, you can define
  a workflow without knowing where exactly the record collection is being
  hosted. This is only needed when actually interpreting the workflow.
- Using effects makes it very easy to test your workflow in a mock environment,
  by changing the interpreter for your effects.
- `IO` actions are opaque to inspection, and so hard to visualise. Providing your
  own effects, on the other hand, lets you fully visualise what's happening in a
  workflow.

So far, all of our examples have used the type `SimpleFlow a b`. `SimpleFlow` is
a type alias for the fully general type `Flow`::

  -- | A workflow taking input of type 'a' and producing output of type 'b'.
  --   This workflow may include user-defined effects of type 'eff' and
  --   raise exceptions of type 'ex'.
  type Flow eff ex a b
  type SimpleFlow = Flow NoEffect SomeException

To include the `RecordCollectionAction`, you can define a new type for your flow::

  type MyFlow = Flow RecordCollectionAction SomeException

To run the flow, you must also provide an interpreter for your effects. This is
a function of type `forall a b. eff a b -> AsyncA IO a b`. Here's an example of
an interpreter for the `RecordCollectionAction` type which just logs what's
happening::

  runRecordCollectionAction :: RecordCollectionAction a b -> AsyncA IO a b
  runRecordCollectionAction Insert = AsyncA $ \rec -> putStrLn $ "Inserting " ++ show rec
  runRecordCollectionAction Select = AsyncA $ \ix -> do
    putStrLn $ "Selecting " ++ show ix
    -- Fail to find anything  in this mock interpreter
    return Nothing
  runRecordCollectionAction Delete = AsyncA $ \ix -> putStrLn $ "Deleting " ++ show ix

Having defined the interpreter, you can use it in place of `runNoEffect`, as in the
example above::

  CS.withStore [absdir|/tmp/funflow|] $ \store -> do
    runFlow SQLite [absfile|/tmp/coordinator.db|] store runRecordCollectionAction 123123 flow input

.. [1] Technically, it lifts it to the more general type `Flow eff ex a b`, but
       that full generality is not needed here.
.. [2] This is heavily inspired by the nix_ package manager.
.. _nix: https://nixos.org/nix
.. _CAS: https://en.wikipedia.org/wiki/Content-addressable_storage
.. _arrows: https://www.haskell.org/arrows/
.. _Docker: https://www.docker.com
.. _Control.Funflow.Exec.Simple: ./funflow/src/Control/Funflow/Exec/Simple.hs
.. _announcement: https://www.tweag.io/posts/2018-04-25-funflow.html
