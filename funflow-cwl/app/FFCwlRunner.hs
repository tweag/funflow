{-# LANGUAGE GADTs       #-}
{-# LANGUAGE QuasiQuotes #-}


module Main ( main ) where

import Control.Exception ( displayException )
import Control.Applicative
import Data.Monoid ((<>))
import qualified Options.Applicative as Opt
import qualified Database.Redis as R
import qualified Data.ByteString as B
import Network.Socket (HostName, PortNumber )


import           Data.HList
import           Data.ErrorMonad
import           Control.Funflow.CWL.Run
  ( tryRun
  , Args (..)
  , UseCoord (..)
  )



main :: IO ()
main = do
  args <- parseArgs
  let run = tryRun args
  let ioRun = runIOErrM run
  result <- ioRun
  case result of
    Left err -> do
      putStrLn "Execution failed: \n"
      putStrLn $ displayException err
    Right (SomeHL outputList) -> do
      putStrLn "Execution succeeded."
      putStrLn "List of outputs (in the content store): \n"
      putStrLn $ show outputList
      putStrLn "Access these outputs by going to the \
        \folder item/<content hash> inside the selected content store."



-- # Parsing Arguments
--------------------------------------------------------------------------------

parseArgs :: IO Args
parseArgs = Opt.execParser $ Opt.info (argsParser Opt.<**> Opt.helper)
  $  Opt.fullDesc
  <> Opt.progDesc
      "Execute CWL workflows or CWL tools.\n\
      \WARNING: All Hints are ignored."
  <> Opt.header "ffcwlrunner - A CWL implementation using Funflow"


argsParser :: Opt.Parser Args
argsParser = Args
  <$> cwlFileParser
  <*> cwlJobParser
  <*> storeParser
  <*> Opt.hsubparser
       ( Opt.command "redis"
           (Opt.info (Opt.helper <*> redisParser)
             (Opt.progDesc "Use Redis coordinator.") )
       <> Opt.command "sqlite"
           (Opt.info (Opt.helper <*> sqliteParser)
             (Opt.progDesc "Use SQLite coordinator."))
       <> Opt.command "memory"
           (Opt.info (pure UseMem) $
             Opt.progDesc "Use internal memory coordinator")
       <> Opt.metavar "COORDINATOR"
       <> Opt.commandGroup "Available coordinators:")
  where

    cwlFileParser :: Opt.Parser FilePath
    cwlFileParser = Opt.strArgument $
      (  Opt.help "The cwl tool or workflow file."
      <> Opt.metavar "CWL-FILE"
      <> Opt.completer (Opt.bashCompleter "filenames")
      )

    cwlJobParser :: Opt.Parser FilePath
    cwlJobParser = Opt.strArgument $
      (  Opt.help "The yaml cwl job file."
      <> Opt.metavar "CWL-JOB"
      <> Opt.completer (Opt.bashCompleter "filenames")
      )


    storeParser :: Opt.Parser String
    storeParser = Opt.strArgument
      $  Opt.metavar "STOREDIR"
      <> Opt.help "Path to the root of the content store."


    redisParser :: Opt.Parser UseCoord
    redisParser = useRedis
      <$> Opt.strArgument
        (  Opt.metavar "HOSTNAME"
        <> Opt.help "Host for the Redis instance." )
      <*> Opt.argument Opt.auto
        (  Opt.metavar "PORT"
        <> Opt.help "Port for the Redis instance." )
      <*> optional (Opt.argument Opt.auto
        (  Opt.metavar "PASSWORD"
        <> Opt.help "Password for the Redis instance, if needed." ))

    useRedis ::
      HostName -> PortNumber -> Maybe B.ByteString -> UseCoord
    useRedis host port pw = UseRedis $ R.defaultConnectInfo
      { R.connectHost = host
      , R.connectPort = R.PortNumber port
      , R.connectAuth = pw
      }

    sqliteParser :: Opt.Parser UseCoord
    sqliteParser = UseSQLite
      <$> Opt.strArgument
        (  Opt.metavar "SQLFILE"
        <> Opt.help "Path to SQLite database file." )
