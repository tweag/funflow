{-# LANGUAGE GADTs       #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeInType #-}


{-|
Module      : Run
Description : This is the top level module of this library that runs 
  a cwl tool or workflow.

This is the top level module of this library that runs
a cwl tool or workflow.

-}



module Control.Funflow.CWL.Run
  ( tryRun
  , Args (..)
  , UseCoord (..)
  )
where


import Control.Funflow
import Path    ( (</>), parseRelDir, parseAbsDir )
import Path.IO ( getCurrentDir )
import Data.Yaml
import Control.Concurrent.Async ( withAsync )

import Control.Funflow.Exec.Simple ( runSimpleFlow )
import Control.Funflow.External.Executor ( executeLoop )
import Control.Funflow.External.Coordinator
import Control.Funflow.External.Coordinator.Memory
import Control.Funflow.External.Coordinator.SQLite
import Control.Funflow.External.Coordinator.Redis
import qualified Control.Funflow.ContentStore as CS


import  Control.Funflow.CWL.Convert
    ( SomeJob (..)
    , convertCWLFile
    , convertJob
    )
import Control.Funflow.CWL.Convert.Wiring  ( typedCompose )
import Control.Funflow.CWL.Types
import Control.Funflow.CWL.PreProcess
import Data.HList
import Data.ErrorMonad




-- * Command line arguments
--------------------------------------------------------------------------------

-- | These are the command line arguments.
data Args = Args
  { cwlFilePath :: FilePath -- ^Relative file path to cwl file
  , jobFilePath :: FilePath -- ^Relative file path to job file
  , storePath   :: FilePath -- ^Relative file path for store directory
  , coordinator :: UseCoord -- ^AST for making a coordinator
  }

-- | A specification of which coordinator to use.
data UseCoord
  = UseRedis (Config Redis)
  | UseSQLite FilePath
  | UseMem


--------------------------------------------------------------------------------



-- * Top Level Runner
--------------------------------------------------------------------------------

-- | Given the command line arguments, execute
-- the input cwl tool or workflow with the job file and
-- produce an existential type for the output heterogeneous list.
tryRun :: Args -> IOErrM SomeHL
tryRun (Args cwlpath jobpath store useCoord) = do
  (cwlVal, jobVal) <- preProcess cwlpath jobpath
  (cwlfile, job) <- tryParse cwlVal jobVal
  SomeTask task <- tryCompile cwlfile job
  SomeCoord (c,conf) <- tryCoord useCoord

  let rsf = runSimpleFlow
  let flowWstore = \cs -> rsf c conf cs task HNil
  let loopWstore = \cs -> executeLoop c conf cs

  storeRelDir <- parseRelDir store
  cwd <- getCurrentDir
  let storeAbsDir = cwd </> storeRelDir

  let run = CS.withStore storeAbsDir $ \cs ->
        do
          let flow = flowWstore cs
          let loop = loopWstore cs
          withAsync loop $ const flow

  let runM = injectIOEither run
  outHList <- runM
  return $ SomeHL outHList







-- * The steps to execute a cwl workflow or tool.
--------------------------------------------------------------------------------

-- | Try to parse two JSON values into a 'CWLFile' and 'Job'
-- respectively.
tryParse :: Value -> Value -> IOErrM (CWLFile, Job)
tryParse cwlfileVal jobVal = do
  cwlfile :: CWLFile <- parseValue cwlfileVal
  job :: Job <- parseValue jobVal
  return (cwlfile, job)
  where
    parseValue :: FromJSON a => Value -> IOErrM a
    parseValue v = case parseEither parseJSON v of
                    Left errmsg -> throwM $ ErrMsg $ errmsg
                    Right a -> return a


-- | An existential type for a Flow that takes
-- no arguments, i.e., a /task/.
data SomeTask where
  SomeTask :: LShowable l =>
    HList '[] ==> HList l -> SomeTask

-- | Try to compile the 'CWLFile' and feed it the compiled
-- 'Job' inputs to create a task.
tryCompile :: CWLFile -> Job -> IOErrM SomeTask
tryCompile cwlFile job = do
  SomeFlow flow inSch _ <- convertCWLFile cwlFile
  SomeJob jobInpFlow jobSch <- convertJob job
  let comp = injectErrM $
        typedCompose jobInpFlow jobSch flow inSch
  task <- comp
  return $ SomeTask task




-- | An existential coordinator type.
data SomeCoord where
  SomeCoord :: Coordinator c => (c, Config c) -> SomeCoord

-- | Pick a coordinator using the command line argument.
tryCoord :: UseCoord -> IOErrM SomeCoord
tryCoord UseMem = do
  memHook <- lift createMemoryCoordinator
  return $ SomeCoord (MemoryCoordinator, memHook)
tryCoord (UseRedis conf) = return $ SomeCoord (Redis, conf)
tryCoord (UseSQLite path) = do
  absDirPath <- parseAbsDir path
  return $ SomeCoord (SQLite, absDirPath)




