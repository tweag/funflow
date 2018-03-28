{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
-- | Executor for external tasks.
module Control.FunFlow.External.Executor where

import           Control.Concurrent                   (threadDelay)
import           Control.Exception.Safe
import           Control.Concurrent.Async
import qualified Control.FunFlow.ContentStore         as CS
import           Control.FunFlow.External
import           Control.FunFlow.External.Coordinator
import           Control.Lens
import           Control.Monad                        (forever, when)
import           Control.Monad.Trans                  (lift)
import           Control.Monad.Trans.Maybe
import qualified Data.ByteString                      as BS
import qualified Data.Text                            as T
import           Katip                                as K
import           Network.HostName
import           Path
import           Path.IO
import           System.Clock
import           System.Exit                          (ExitCode (..))
import           System.IO                            (Handle, IOMode (..),
                                                       openFile, stderr, stdout)
import           System.Posix.Env                     (getEnv)
import           System.Posix.User
import           System.Process

data ExecutionResult =
    -- | The result already exists in the store and there is no need
    --   to execute. This is also returned if the job is already running
    --   elsewhere.
    Cached
    -- | The computation is already running elsewhere. This is probably
    --   indicative of a bug, because the coordinator should only allow one
    --   instance of a task to be running at any time.
  | AlreadyRunning
    -- | Execution completed successfully after a certain amount of time.
  | Success TimeSpec
    -- | Execution failed with the following exit code.
    --   TODO where should logs go?
  | Failure TimeSpec Int

-- | Execute an individual task.
execute :: CS.ContentStore -> TaskDescription -> KatipContextT IO ExecutionResult
execute store td = do
  status <- CS.withConstructIfMissing store (td ^. tdOutput) $ \fp -> do
    (fpOut, hOut) <- lift $
      CS.createMetadataFile store (td ^. tdOutput) [relfile|stdout|]
    (fpErr, hErr) <- lift $
      CS.createMetadataFile store (td ^. tdOutput) [relfile|stderr|]
    let
      withFollowOutput m
        | td ^. tdTask . etWriteToStdOut
        = withAsync (followFile fpErr stderr) $ \_ -> m
        | otherwise
        = withAsync (followFile fpErr stderr) $ \_ ->
          withAsync (followFile fpOut stdout) $ \_ -> m
      cmd = T.unpack $ td ^. tdTask . etCommand
      procSpec params = (proc cmd $ T.unpack <$> params)
        { cwd = Just (fromAbsDir fp)
        , close_fds = True
        , std_err = UseHandle hErr
        , std_out = UseHandle hOut
        }
      convParam = ConvParam
        { convPath = pure . CS.itemPath store
        , convEnv = \e -> T.pack <$> MaybeT (getEnv $ T.unpack e)
        , convUid = lift getEffectiveUserID
        , convGid = lift getEffectiveGroupID
        , convOut = pure fp
        }
    mbParams <- lift $ runMaybeT $
      traverse (paramToText convParam) (td ^. tdTask . etParams)
    params <- case mbParams of
      Nothing     -> fail "A parameter was not ready"
      Just params -> return params

    start <- lift $ getTime Monotonic
    let theProc = procSpec params
    katipAddNamespace "process" . katipAddContext (sl "processId" $ show theProc) $ do
      $(logTM) InfoS "Executing"
      lift $ withCreateProcess theProc $ \_ _ _ ph ->
        -- Error output should be displayed on our stderr stream
        withFollowOutput $ do
          exitCode <- waitForProcess ph
          end <- getTime Monotonic
          case exitCode of
            ExitSuccess   -> do
              when (td ^. tdTask . etWriteToStdOut) $
                copyFile fpOut (fp </> [relfile|out|])
              return $ Right (diffTimeSpec start end)
            ExitFailure i ->
              return $ Left (diffTimeSpec start end, i)
  case status of
    CS.Missing (t, ec) -> return (Failure t ec)
    CS.Pending () -> return AlreadyRunning
    CS.Complete (Nothing, _) -> return Cached
    CS.Complete (Just t, _) -> return (Success t)

-- | Execute tasks forever
executeLoop :: forall c. Coordinator c
            => c
            -> Config c
            -> CS.ContentStore
            -> IO ()
executeLoop coord cfg store =
  executeLoopWithScribe coord cfg store =<<
    mkHandleScribe ColorIfTerminal stdout InfoS V2

-- | Same as 'executeLoop', but allows specifying a custom 'Scribe' for logging
executeLoopWithScribe :: forall c. Coordinator c
                      => c
                      -> Config c
                      -> CS.ContentStore
                      -> Scribe
                      -> IO ()
executeLoopWithScribe _ cfg store handleScribe = do
  let mkLogEnv = registerScribe "stdout" handleScribe defaultScribeSettings =<< initLogEnv "FFExecutorD" "production"
  bracket mkLogEnv closeScribes $ \le -> do
    let initialContext = ()
        initialNamespace = "executeLoop"

    runKatipContextT le initialContext initialNamespace $ do
      $(logTM) InfoS "Initialising connection to coordinator."
      hook :: Hook c <- lift $ initialise cfg
      executor <- lift $ Executor <$> getHostName

      -- Types of completion/status updates
      let fromCache = Completed $ ExecutionInfo executor 0
          afterTime t = Completed $ ExecutionInfo executor t
          afterFailure t i = Failed (ExecutionInfo executor t) i
          -- Known failures that do not affect the executors ability
          -- to execute further tasks will be logged and ignored.
          handleFailures = handle $ \(e::CS.StoreError) ->
            -- Certain store errors can occur if an item is forcibly removed
            -- while the executor is constructing it or picked up a
            -- corresponding outdated task from the queue.
            -- XXX: The store should distinguish between recoverable
            --   and unrecoverable errors.
            $(logTM) WarningS . ls $ displayException e

      forever $ handleFailures $ do
        $(logTM) DebugS "Awaiting task from coordinator."
        mtask <- popTask hook executor
        case mtask of
          Nothing -> lift $ threadDelay 1000000
          Just task -> katipAddContext (sl "task" $ task ^. tdOutput) $ do
            $(logTM) DebugS "Checking task"
            res <- execute store task
            case res of
              Cached      -> updateTaskStatus hook (task ^. tdOutput) fromCache
              Success t   -> updateTaskStatus hook (task ^. tdOutput) $ afterTime t
              Failure t i -> updateTaskStatus hook (task ^. tdOutput) $ afterFailure t i
              AlreadyRunning -> return ()

-- | @followFile in out@ follows the file @in@
--   and prints contents to @out@ as they appear.
--   The file must exist. Doesn't handle file truncation.
followFile :: Path Abs File -> Handle -> IO ()
followFile infile outhandle = do
  inhandle <- openFile (fromAbsFile infile) ReadMode
  forever $ do
    some <- BS.hGetSome inhandle 4096
    if BS.null some then
      threadDelay 100000
    else
      BS.hPut outhandle some
