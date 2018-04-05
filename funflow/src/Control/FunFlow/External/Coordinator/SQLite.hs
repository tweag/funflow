{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE QuasiQuotes       #-}

module Control.FunFlow.External.Coordinator.SQLite
  ( SQLite (..)
  ) where

import           Control.Concurrent                   (threadDelay)
import           Control.Exception.Safe
import           Control.FunFlow.ContentHashable
import           Control.FunFlow.External
import           Control.FunFlow.External.Coordinator
import           Control.FunFlow.Lock
import           Control.Lens
import           Control.Monad.IO.Class
import qualified Data.Aeson                           as Json
import qualified Data.ByteString.Char8                as C8
import qualified Data.Text                            as T
import           Data.Typeable                        (Typeable)
import qualified Database.SQLite.Simple               as SQL
import qualified Database.SQLite.Simple.FromField     as SQL
import qualified Database.SQLite.Simple.Ok            as SQL
import qualified Database.SQLite.Simple.ToField       as SQL
import           Path
import           Path.IO
import           System.Clock

-- | SQLite coordinator tag.
data SQLite = SQLite

-- | SQLite coordinator hook.
data SQLiteHook = SQLiteHook
  { _sqlConn :: SQL.Connection
  , _sqlLock :: Lock
  }
makeLenses ''SQLiteHook

-- | Take the lock and run the given action on the SQLite connection.
withSQLite :: SQLiteHook -> (SQL.Connection -> IO a) -> IO a
withSQLite hook action = withLock (hook^.sqlLock) $ action (hook^.sqlConn)

-- | Enumeration of possible 'TaskStatus' cases for SQLite status column.
data SqlTaskStatus
  = SqlPending
  | SqlRunning
  | SqlCompleted
  | SqlFailed
  deriving Enum
instance SQL.FromField SqlTaskStatus where
  fromField field = do
    n <- SQL.fromField field
    pure $! toEnum n
instance SQL.ToField SqlTaskStatus where
  toField = SQL.toField . fromEnum

-- | Wrapper around 'Executor' for SQLite serialization.
newtype SqlExecutor = SqlExecutor { getSqlExecutor :: Executor }
instance SQL.FromField SqlExecutor where
  fromField field = SqlExecutor . Executor <$> SQL.fromField field
instance SQL.ToField SqlExecutor where
  toField (SqlExecutor (Executor host)) = SQL.toField host

-- | SQLite task info query result.
data SqlTaskInfo = SqlTaskInfo
  { _stiStatus   :: SqlTaskStatus
  , _stiExecutor :: Maybe SqlExecutor
  , _stiElapsed  :: Maybe Integer
  , _stiExitCode :: Maybe Int
  }
makeLenses '' SqlTaskInfo
instance SQL.FromRow SqlTaskInfo where
  fromRow = SqlTaskInfo
    <$> SQL.field
    <*> SQL.field
    <*> SQL.field
    <*> SQL.field

-- | Wrapper around 'ExternalTask' for SQLite serialization.
newtype SqlExternal = SqlExternal { getSqlExternal :: ExternalTask }
instance SQL.FromField SqlExternal where
  fromField field = do
    bs <- SQL.fromField field
    case Json.eitherDecode bs of
      Left err -> SQL.Errors [toException $ DecodingError "task" err]
      Right x  -> pure $! SqlExternal x
instance SQL.ToField SqlExternal where
  toField = SQL.toField . Json.encode . getSqlExternal

-- | Wrapper around 'TaskDescription' for SQLite serialization.
newtype SqlTask = SqlTask TaskDescription
instance SQL.FromRow SqlTask where
  fromRow = do
    output <- SQL.field
    SqlExternal task <- SQL.field
    pure $! SqlTask $! TaskDescription
      { _tdOutput = output
      , _tdTask = task
      }

-- | Errors that can occur when interacting with the SQLite coordinator.
data SQLiteCoordinatorError
    -- | @MissingDBTaskEntry output field@
    --   The task database entry is missing a field.
  = MissingDBTaskEntry ContentHash T.Text
    -- | @DecodingError field error@
    --   Failed to decode the field.
  | DecodingError T.Text String
    -- | @NonRunningTask output@
    --   The task is not running.
  | NonRunningTask ContentHash
    -- | @IllegalStatusUpdate output status@
    --   Cannot update the status of the task.
  | IllegalStatusUpdate ContentHash TaskStatus
  deriving (Show, Typeable)
instance Exception SQLiteCoordinatorError where
  displayException (MissingDBTaskEntry output field) =
    "Missing field in SQLite task entry '"
    ++ T.unpack field
    ++ "' for output "
    ++ C8.unpack (encodeHash output)
  displayException (DecodingError field err) =
    "Failed to decode field '"
    ++ T.unpack field
    ++ "': "
    ++ err
  displayException (NonRunningTask output) =
    "Task was not running when expected: "
    ++ C8.unpack (encodeHash output)
  displayException (IllegalStatusUpdate output status) =
    "Illegal status update for "
    ++ C8.unpack (encodeHash output)
    ++ ": "
    ++ show status

-- | Helper for @NULL@ valued data-base fields.
--
-- Throws 'MissingDBTaskEntry' on 'Nothing', otherwise returns the value.
fromMaybeField :: MonadIO m => ContentHash -> T.Text -> Maybe a -> m a
fromMaybeField output f = \case
  Nothing -> liftIO $ throwIO $ MissingDBTaskEntry output f
  Just x -> pure x

-- | Unlifted version of 'taskInfo'.
taskInfo' :: SQLiteHook -> ContentHash -> IO TaskInfo
taskInfo' hook output = do
  r <- withSQLite hook $ \conn -> SQL.queryNamed conn
    "SELECT status, executor, elapsed, exit_code FROM tasks\
    \ WHERE\
    \  output = :output"
    [ ":output" SQL.:= output ]
  case r of
    [] -> pure UnknownTask
    (ti:_) -> case ti^.stiStatus of
      SqlPending -> pure $! KnownTask Pending
      SqlRunning -> do
        executor <- fromMaybeField output "executor" (ti^.stiExecutor)
        pure $! KnownTask $! Running ExecutionInfo
          { _eiExecutor = getSqlExecutor executor
          , _eiElapsed = fromNanoSecs 0
          }
      SqlCompleted -> do
        executor <- fromMaybeField output "executor" (ti^.stiExecutor)
        elapsed <- fromMaybeField output "elapsed" (ti^.stiElapsed)
        pure $! KnownTask $! Completed ExecutionInfo
          { _eiExecutor = getSqlExecutor executor
          , _eiElapsed = fromNanoSecs elapsed
          }
      SqlFailed -> do
        executor <- fromMaybeField output "executor" (ti^.stiExecutor)
        elapsed <- fromMaybeField output "elapsed" (ti^.stiElapsed)
        exitCode <- fromMaybeField output "exit_code" (ti^.stiExitCode)
        pure $! KnownTask $! Failed
          ExecutionInfo
            { _eiExecutor = getSqlExecutor executor
            , _eiElapsed = fromNanoSecs elapsed
            }
          exitCode

instance Coordinator SQLite where
  type Config SQLite = Path Abs Dir
  type Hook SQLite = SQLiteHook

  initialise dir = liftIO $ do
    createDirIfMissing True dir
    lock <- openLock (dir </> [reldir|lock|])
    withLock lock $ do
      conn <- SQL.open $ fromAbsFile (dir </> [relfile|db.sqlite|])
      SQL.execute_ conn
        "CREATE TABLE IF NOT EXISTS\
        \  tasks\
        \  ( output TEXT PRIMARY KEY\
        \  , status INT NOT NULL\
        \  , executor TEXT\
        \  , elapsed INT\
        \  , exit_code INT\
        \  , task TEXT NOT NULL\
        \  )"
      return SQLiteHook
        { _sqlConn = conn
        , _sqlLock = lock
        }

  submitTask hook td = liftIO $
    withSQLite hook $ \conn -> SQL.executeNamed conn
      "INSERT OR REPLACE INTO\
      \  tasks (output, status, task)\
      \ VALUES\
      \  (:output, :status, :task)"
      [ ":output" SQL.:= (td^.tdOutput)
      , ":status" SQL.:= SqlPending
      , ":task" SQL.:= SqlExternal (td^.tdTask)
      ]

  queueSize hook = liftIO $ do
    [[n]] <- withSQLite hook $ \conn -> SQL.queryNamed conn
      "SELECT COUNT(*) FROM tasks WHERE status = :pending"
      [ ":pending" SQL.:= SqlPending ]
    return n

  taskInfo hook output = liftIO $
    taskInfo' hook output

  popTask hook executor = liftIO $
    withSQLite hook $ \conn -> SQL.withTransaction conn $ do
      r <- SQL.queryNamed conn
        "SELECT output, task FROM tasks\
        \ WHERE\
        \  status = :pending\
        \ LIMIT 1"
        [ ":pending" SQL.:= SqlPending ]
      case r of
        [] -> pure Nothing
        (SqlTask td:_) -> do
          SQL.executeNamed conn
            "UPDATE tasks\
            \ SET\
            \  status = :status,\
            \  executor = :executor\
            \ WHERE\
            \  output = :output"
            [ ":status" SQL.:= SqlRunning
            , ":executor" SQL.:= SqlExecutor executor
            , ":output" SQL.:= td^.tdOutput
            ]
          pure $! Just td

  awaitTask hook output = liftIO $ loop
    where
      -- XXX: SQLite has callback mechanisms built-in (e.g. @sqlite3_commit_hook@).
      --   Unfortunately, @direct-sqlite@, which @sqlite-simple@ builds on top of,
      --   doesn't expose this functionality at the moment.
      loop = taskInfo' hook output >>= \case
        KnownTask Pending -> sleep >> loop
        KnownTask (Running _) -> sleep >> loop
        ti -> pure ti
      sleep = liftIO $ threadDelay oneSeconds
      oneSeconds = 1000000

  updateTaskStatus hook output ts = liftIO $
    withSQLite hook $ \conn -> SQL.withTransaction conn $ do
      r <- SQL.queryNamed conn
        "SELECT status FROM tasks\
        \ WHERE\
        \  output = :output"
        [ ":output" SQL.:= output ]
      case r of
        [SqlRunning]:_ -> case ts of
          Completed ei -> SQL.executeNamed conn
            "UPDATE tasks\
            \ SET\
            \  status = :completed,\
            \  elapsed = :elapsed\
            \ WHERE\
            \  output = :output"
            [ ":completed" SQL.:= SqlCompleted
            , ":elapsed" SQL.:= toNanoSecs (ei^.eiElapsed)
            , ":output" SQL.:= output
            ]
          Failed ei exitCode -> SQL.executeNamed conn
            "UPDATE tasks\
            \ SET\
            \  status = :failed,\
            \  elapsed = :elapsed,\
            \  exit_code = :exit_code\
            \ WHERE\
            \  output = :output"
            [ ":failed" SQL.:= SqlFailed
            , ":elapsed" SQL.:= toNanoSecs (ei^.eiElapsed)
            , ":exit_code" SQL.:= exitCode
            , ":output" SQL.:= output
            ]
          Pending -> SQL.executeNamed conn
            "UPDATE tasks\
            \ SET\
            \  status = :pending\
            \ WHERE\
            \  output = :output"
            [ ":pending" SQL.:= SqlPending
            , ":output" SQL.:= output
            ]
          Running _ -> throwIO $ IllegalStatusUpdate output ts
        _ -> throwIO $ NonRunningTask output

  dropTasks hook = liftIO $
    withSQLite hook $ \conn ->
      SQL.executeNamed conn
        "DELETE FROM tasks\
        \ WHERE\
        \  status = :pending"
        [ ":pending" SQL.:= SqlPending ]
