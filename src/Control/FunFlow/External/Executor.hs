{-# LANGUAGE ScopedTypeVariables #-}
-- | Executor for external tasks.
module Control.FunFlow.External.Executor where

import           Control.Concurrent                   (threadDelay)
import           Control.FunFlow.ContentStore
import           Control.FunFlow.External
import           Control.FunFlow.External.Coordinator
import           Control.Lens
import           Control.Monad                        (forever)
import qualified Data.Text                            as T
import           Network.HostName
import           System.Clock
import           System.Exit                          (ExitCode (..))
import           System.FilePath                      ((</>))
import           System.IO                            (IOMode (..), openFile)
import           System.Process

data ExecutionResult =
    -- | The result already exists in the store and there is no need
    --   to execute. This is also returned if the job is already running
    --   elsewhere.
    Cached
    -- | Execution completed successfully after a certain amount of time.
  | Success TimeSpec
    -- | Execution failed with the following exit code.
    --   TODO where should logs go?
  | Failure TimeSpec Int

-- | Execute an individual task.
execute :: ContentStore -> TaskDescription -> IO ExecutionResult
execute store td = do
  instruction <- constructIfMissing store (td ^. tdOutput)
  case instruction of
    Wait -> return Cached
    Consume _ -> return Cached
    Construct fp -> let
        defaultProc = proc (T.unpack $ td ^. tdTask . etCommand)
                       (T.unpack <$> td ^. tdTask . etParams)
        procSpec out = defaultProc {
            cwd = Just fp
          , close_fds = True
            -- Error output should be displayed on our stderr stream
          , std_err = Inherit
          , std_out = out
          }
      in do
        out <-
          if (td ^. tdTask . etWriteToStdOut)
          then UseHandle <$> openFile (fp </> "out") WriteMode
          else return Inherit

        start <- getTime Monotonic
        (_, _, _, ph) <- createProcess $ procSpec out
        exitCode <- waitForProcess ph
        end <- getTime Monotonic
        case exitCode of
          ExitSuccess   -> do
            markComplete store (td ^. tdOutput)
            return $ Success (diffTimeSpec start end)
          ExitFailure i -> do
            removeFailed store (td ^. tdOutput)
            return $ Failure (diffTimeSpec start end) i

-- | Execute tasks forever
executeLoop :: forall c. Coordinator c
            => c
            -> Config c
            -> FilePath
            -> IO ()
executeLoop _ cfg sroot = do
  hook :: Hook c <- initialise cfg
  executor <- Executor <$> getHostName
  store <- initialize sroot

  -- Types of completion/status updates
  let fromCache = Completed $ ExecutionInfo executor 0
      afterTime t = Completed $ ExecutionInfo executor t
      afterFailure t i = Failed (ExecutionInfo executor t) i

  forever $ do
    mtask <- popTask hook executor
    case mtask of
      Nothing -> threadDelay 1000000
      Just task -> do
        res <- execute store task
        updateTaskStatus hook (task ^. tdOutput) $ case res of
          Cached      -> fromCache
          Success t   -> afterTime t
          Failure t i -> afterFailure t i
