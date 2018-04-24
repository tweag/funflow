{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Funflow.ContentStore.Notify.BSD
  ( Notifier
  , initNotifier
  , killNotifier

  , Watch
  , addDirWatch
  , removeDirWatch
  ) where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception.Safe
import           Control.Monad
import           Data.Typeable
import           Foreign.Ptr
import           System.KQueue
import           System.Posix.IO
import           System.Posix.Types

type Notifier = KQueue

data NotifierException
  = RequiresThreadedRuntime
  deriving (Show, Typeable)
instance Exception NotifierException where
  displayException = \case
    -- XXX: Compile time check?
    RequiresThreadedRuntime ->
      "Threaded runtime required! Please rebuild with -threaded flag."

initNotifier :: IO Notifier
initNotifier = do
  unless rtsSupportsBoundThreads $ throwIO RequiresThreadedRuntime
  kqueue

killNotifier :: Notifier -> IO ()
killNotifier _ = return ()

data Watch = Watch KQueue Fd (Async ())

addDirWatch :: Notifier -> FilePath -> IO () -> IO Watch
addDirWatch kq dir f = do
  fd <- openFd dir ReadOnly Nothing defaultFileFlags
  a <- async $ do
    let event = KEvent
          { ident = fromIntegral fd
          , evfilter = EvfiltVnode
          , flags = [EvAdd]
          , fflags = [NoteDelete, NoteAttrib, NoteRename, NoteRevoke]
          , data_ = 0
          , udata = nullPtr
          }
        loop = do
          chgs <- kevent kq [event] 1 Nothing
          unless (null chgs) f
          loop
    loop `finally` closeFd fd
  return $! Watch kq fd a

removeDirWatch :: Watch -> IO ()
removeDirWatch (Watch kq fd a) = do
  closeFd fd
  let event = KEvent
        { ident = fromIntegral fd
        , evfilter = EvfiltVnode
        , flags = [EvDelete]
        , fflags = []
        , data_ = 0
        , udata = nullPtr
        }
  void (kevent kq [event] 0 Nothing)
    `catch` \(_ :: KQueueException) -> return ()
  cancel a
