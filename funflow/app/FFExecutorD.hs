{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

import           Control.Applicative
import           Control.Concurrent                          (myThreadId)
import           Control.Concurrent.MVar
import           Control.Exception.Base                      (AsyncException (UserInterrupt))
import           Control.Exception.Safe
import qualified Control.Funflow.ContentStore                as CS
import           Control.Funflow.External.Coordinator
import           Control.Funflow.External.Coordinator.Redis
import           Control.Funflow.External.Coordinator.SQLite
import           Control.Funflow.External.Executor
import           Control.Monad                               (void)
import           Data.Monoid                                 ((<>))
import qualified Database.Redis                              as R
import qualified Options.Applicative                         as Opt
import           Path
import           System.Clock
import           System.IO
import qualified System.Posix.Signals                        as Signals


data UseCoord
  = UseRedis (Config Redis)
  | UseSQLite FilePath

data Args = Args
  { storePath   :: FilePath
  , coordinator :: UseCoord
  }

argsParser :: Opt.Parser Args
argsParser = Opt.subparser
  $  Opt.command "redis"
      (Opt.info
        (redisParser Opt.<**> Opt.helper)
        (Opt.progDesc "Use Redis coordinator."))
  <> Opt.command "sqlite"
      (Opt.info
        (sqliteParser Opt.<**> Opt.helper)
        (Opt.progDesc "Use SQLite coordinator."))
  <> Opt.metavar "COORDINATOR"
  <> Opt.commandGroup "Available coordinators:"
  where
    storeParser = Opt.strArgument
      $  Opt.metavar "STOREDIR"
      <> Opt.help "Path to the root of the content store."
    redisParser = useRedis
      <$> storeParser
      <*> Opt.strArgument
        (  Opt.metavar "HOSTNAME"
        <> Opt.help "Host for the Redis instance." )
      <*> Opt.argument Opt.auto
        (  Opt.metavar "PORT"
        <> Opt.help "Port for the Redis instance." )
      <*> optional (Opt.argument Opt.auto
        (  Opt.metavar "PASSWORD"
        <> Opt.help "Password for the Redis instance, if needed." ))
    useRedis store host port pw = Args store $ UseRedis R.defaultConnectInfo
      { R.connectHost = host
      , R.connectPort = R.PortNumber port
      , R.connectAuth = pw
      }
    sqliteParser = useSQLite
      <$> storeParser
      <*> Opt.strArgument
        (  Opt.metavar "SQLFILE"
        <> Opt.help "Path to SQLite database file." )
    useSQLite store sqlite = Args store $ UseSQLite sqlite

parseArgs :: IO Args
parseArgs = Opt.execParser $ Opt.info (argsParser Opt.<**> Opt.helper)
  $  Opt.fullDesc
  <> Opt.progDesc
      "Await and execute funflow external tasks on the given coordinator."
  <> Opt.header "ffexecutord - Funflow task executor"

data HandlerState
  = Initial
  | Terminating TimeSpec TimeSpec

data HandlerInstruction
  = Terminate
  | Ignore
  | Message

main :: IO ()
main = do
  config <- parseArgs

  -- Configure custom interrupt handler.
  -- GHC's default interrupt handler works only one time. On the second
  -- interrupt the process will terminate immediately without any cleanup.
  -- This could leave the funflow store and coordinator in an invalid state.
  -- This installs a handler that works on multiple interrupts.
  -- On the first interrupt it will send an async `UserInterrupt` exception to
  -- the main-thread. On further interrupts it will print a message clarifying
  -- that clean-up is ongoing.
  mainThreadId <- myThreadId
  mvState <- newMVar Initial
  let handler name = Signals.Catch $ do
        let msgTime = fromNanoSecs 1000000000
        instr <- modifyMVar mvState $ \case
          Initial -> do
            t <- getTime Monotonic
            return (Terminating t t, Terminate)
          Terminating tsig tmsg -> do
            t' <- getTime Monotonic
            return $
              if t' `diffTimeSpec` tmsg > msgTime then
                (Terminating tsig 0, Message)
              else
                (Terminating tsig tmsg, Ignore)
        case instr of
          Terminate -> throwTo mainThreadId UserInterrupt
          Ignore -> return ()
          Message -> void $ tryAny $ hPutStrLn stderr $
            "Handling " ++ name ++ " signal. Awaiting cleanup."
      installHandler signal name = void $
        Signals.installHandler signal (handler name) Nothing
  installHandler Signals.sigINT "interrupt"
  installHandler Signals.sigQUIT "quit"
  installHandler Signals.sigTERM "terminate"

  -- XXX: Improve handling of invalid paths.
  storePath' <- parseAbsDir (storePath config)
  case coordinator config of
    UseRedis redisConfig -> CS.withStore storePath' $
      executeLoop Redis redisConfig
    UseSQLite sqlitePath -> do
      sqlitePath' <- parseAbsDir sqlitePath
      CS.withStore storePath' $
        executeLoop SQLite sqlitePath'
