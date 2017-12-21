{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

import           Control.Applicative
import qualified Control.FunFlow.ContentStore                as CS
import           Control.FunFlow.External.Coordinator
import           Control.FunFlow.External.Coordinator.Redis
import           Control.FunFlow.External.Coordinator.SQLite
import           Control.FunFlow.External.Executor
import           Data.Monoid                                 ((<>))
import qualified Database.Redis                              as R
import qualified Options.Applicative                         as Opt
import           Path


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

main :: IO ()
main = do
  config <- parseArgs

  -- XXX: Improve handling of invalid paths.
  storePath' <- parseAbsDir (storePath config)
  case coordinator config of
    UseRedis redisConfig -> CS.withStore storePath' $
      executeLoop Redis redisConfig
    UseSQLite sqlitePath -> do
      sqlitePath' <- parseAbsDir sqlitePath
      CS.withStore storePath' $
        executeLoop SQLite sqlitePath'
