{-# LANGUAGE ScopedTypeVariables #-}
-- | Executor for external tasks.
module Control.FunFlow.External.Executor where

import           Control.Exception                    (IOException, try)
import qualified Control.FunFlow.ContentStore         as CS
import           Control.FunFlow.External
import           Control.FunFlow.External.Coordinator
import           Control.Lens
import           Control.Monad                        (forever)
import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.Trans.Maybe
import qualified Data.Text                            as T
import           Network.HostName
import           System.Clock
import           System.Exit                          (ExitCode (..))
import           System.FilePath                      ((</>))
import           System.IO                            (IOMode (..), openFile)
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
execute :: CS.ContentStore -> TaskDescription -> IO ExecutionResult
execute store td = do
  instruction <- CS.constructIfMissing store (td ^. tdOutput)
  case instruction of
    CS.Wait -> return AlreadyRunning
    CS.Consume _ -> return Cached
    CS.Construct fp -> let
        cmd = T.unpack $ td ^. tdTask . etCommand
        procSpec params out = (proc cmd $ T.unpack <$> params) {
            cwd = Just fp
          , close_fds = True
            -- Error output should be displayed on our stderr stream
          , std_err = Inherit
          , std_out = out
          }
        convParam = ConvParam
          { convPath = pure . CS.itemPath
          , convEnv = \e -> T.pack <$> MaybeT (getEnv $ T.unpack e)
          , convUid = liftIO $ getEffectiveUserID
          , convGid = liftIO $ getEffectiveGroupID
          , convOut = pure fp
          }
      in do
        mbParams <- runMaybeT $
          traverse (paramToText convParam) (td ^. tdTask . etParams)
        params <- case mbParams of
          -- XXX: Should we block here?
          Nothing -> fail "A parameter was not ready"
          Just params -> return params

        out <-
          if (td ^. tdTask . etWriteToStdOut)
          then UseHandle <$> openFile (fp </> "out") WriteMode
          else return Inherit

        start <- getTime Monotonic
        let theProc = procSpec params out
        -- XXX: Do proper logging
        putStrLn $ "Executing: " ++ show theProc
        mp <- try $ createProcess theProc
        case mp of
          Left (ex :: IOException) -> do
            -- XXX: Do proper logging
            putStrLn $ "Failed: " ++ show ex
            CS.removeFailed store (td ^. tdOutput)
            return $ Failure (diffTimeSpec start start) 2
          Right (_, _, _, ph) -> do
            exitCode <- waitForProcess ph
            end <- getTime Monotonic
            case exitCode of
              ExitSuccess   -> do
                _ <- CS.markComplete store (td ^. tdOutput)
                return $ Success (diffTimeSpec start end)
              ExitFailure i -> do
                CS.removeFailed store (td ^. tdOutput)
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
  store <- CS.initialize sroot

  -- Types of completion/status updates
  let fromCache = Completed $ ExecutionInfo executor 0
      afterTime t = Completed $ ExecutionInfo executor t
      afterFailure t i = Failed (ExecutionInfo executor t) i

  forever $ do
    mtask <- popTask hook executor
    case mtask of
      Nothing -> return ()
      Just task -> do
        res <- execute store task
        case res of
          Cached      -> updateTaskStatus hook (task ^. tdOutput) fromCache
          Success t   -> updateTaskStatus hook (task ^. tdOutput) $ afterTime t
          Failure t i -> updateTaskStatus hook (task ^. tdOutput) $ afterFailure t i
          AlreadyRunning -> return ()
