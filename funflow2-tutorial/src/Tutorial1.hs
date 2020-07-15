{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -F -pgmF inlitpp #-}

```haskell top hide
-- Instances of AskInliterate to display properly things
import Lib ()
```

# funflow2: User Guide

We need to use the following pragmas:

```haskell
{-# LANGUAGE TypeApplications #-}
```

First we import funflow2:

```haskell top
import Funflow (
    Flow
  , pureFlow
  , FlowExecutionConfig (FlowExecutionConfig)
  , commandExecution
  , CommandExecutionHandler (SystemExecutor)
  , runFlow
  )
```

We then define a flow:

```haskell top
flow :: Flow () String
flow = pureFlow $ \() -> "Hello world"
```

although our flow is pure, we still need to pass some configuration to the `runFlow` function:

```haskell top
flowExecutionConfig :: FlowExecutionConfig 
flowExecutionConfig = FlowExecutionConfig {commandExecution = SystemExecutor}
```

Finally we can run our flow:

```haskell eval twocol
runFlow @() @String flowExecutionConfig flow ()
```

## Complete example

```haskell
{-# LANGUAGE TypeApplications #-}

import Funflow (Flow, pureFlow, runFlow)

flow :: Flow () String
flow = pureFlow $ \() -> "Hello world"

flowExecutionConfig :: FlowExecutionConfig 
flowExecutionConfig = FlowExecutionConfig {commandExecution = SystemExecutor}

main = runFlow @() @String flowExecutionConfig flow ()
```