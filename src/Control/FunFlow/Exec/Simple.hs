{-# LANGUAGE Arrows              #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Control.FunFlow.Exec.Simple
  ( runFlow
  , runFlowEx
  , runSimpleFlow
  , withSimpleLocalRunner
  ) where

import           Control.Arrow (returnA)
import           Control.Arrow.Async
import           Control.Arrow.Free                   (eval, type (~>))
import           Control.Concurrent.Async             (wait, withAsync)
import           Control.FunFlow.Base
import           Control.FunFlow.ContentHashable
import qualified Control.FunFlow.ContentStore         as CS
import           Control.FunFlow.External
import           Control.FunFlow.External.Coordinator
import           Control.FunFlow.External.Coordinator.Memory
import           Control.FunFlow.External.Executor    (executeLoop)
import           Control.Monad.Catch                  ( SomeException
                                                      , Exception, onException
                                                      , try)
import qualified Data.ByteString                      as BS
import           Path

-- | Simple evaulation of a flow
runFlowEx :: forall c eff ex a b. (Coordinator c, Exception ex)
          => c
          -> Config c
          -> CS.ContentStore
          -> (eff ~> AsyncA IO) -- ^ Natural transformation from wrapped effects
          -> Flow eff ex a b
          -> a
          -> IO b
runFlowEx _ cfg store runWrapped flow input = do
    hook <- initialise cfg
    runAsyncA (eval (runFlow' hook) flow) input
  where
    simpleOutPath item = toFilePath
      $ CS.itemPath store item </> [relfile|out|]
    withStoreCache :: forall i o. Cacher i o
                   -> AsyncA IO i o -> AsyncA IO i o
    withStoreCache NoCache f = f
    withStoreCache c f = let
        chashOf i = cacherKey c i (cacherUniqueIdent c)
        checkStore = AsyncA $ \chash -> do
          instruction <- CS.constructOrWait store chash
          case instruction of
            CS.Pending a -> do
              update <- wait a
              case update of
                CS.Completed item -> do
                  bs <- BS.readFile $ simpleOutPath item
                  return . Right . cacherReadValue c $ bs
                CS.Failed ->
                  -- XXX: Should we retry locally?
                  fail "Remote process failed to construct item"
            CS.Complete item -> do
              bs <- BS.readFile $ simpleOutPath item
              return . Right . cacherReadValue c $ bs
            CS.Missing fp -> return $ Left fp
        writeStore = AsyncA $ \(chash, fp, res) ->
          do
             BS.writeFile (toFilePath $ fp </> [relfile|out|])
                         . cacherStoreValue c $ res
             _ <- CS.markComplete store chash
             return res
           `onException`
             CS.removeFailed store chash
      in proc i -> do
        let chash = chashOf i
        mcontents <- checkStore -< chash
        case mcontents of
          Right contents -> returnA -< contents
          Left fp -> do
            res <- f -< i
            writeStore -< (chash, fp, res)

    runFlow' :: Hook c -> Flow' eff a1 b1 -> AsyncA IO a1 b1
    runFlow' _ (Step props f) = withStoreCache (cache props)
      $ AsyncA $ \x -> return $ f x
    runFlow' _ (StepIO props f) = withStoreCache (cache props)
      $ AsyncA f
    runFlow' po (External toTask) = AsyncA $ \x -> do
      chash <- contentHash (x, toTask x)
      submitTask po $ TaskDescription chash (toTask x)
      KnownTask _ <- awaitTask po chash
      CS.waitUntilComplete store chash >>= \case
        Nothing -> fail "Remote process failed to construct item"
        Just item -> return item
    runFlow' _ (PutInStore f) = AsyncA $ \x -> do
      chash <- contentHash x
      instruction <- CS.constructOrWait store chash
      case instruction of
        CS.Pending a -> do
          update <- wait a
          case update of
            CS.Completed item -> return item
            CS.Failed ->
              -- XXX: Should we retry locally?
              fail "Remote process failed to construct item"
        CS.Complete item -> return item
        CS.Missing fp ->
          do
            f fp x
            CS.markComplete store chash
          `onException`
          CS.removeFailed store chash
    runFlow' _ (GetFromStore f) = AsyncA $ \item ->
      f $ CS.itemPath store item
    runFlow' _ LookupAliasInStore = AsyncA $ \alias ->
      CS.lookupAlias store alias
    runFlow' _ AssignAliasInStore = AsyncA $ \(alias, item) ->
      CS.assignAlias store alias item
    runFlow' _ (Wrapped props w) = withStoreCache (cache props)
      $ runWrapped w

runFlow :: forall c eff ex a b. (Coordinator c, Exception ex)
        => c
        -> Config c
        -> CS.ContentStore
        -> (eff ~> AsyncA IO) -- ^ Natural transformation from wrapped effects
        -> Flow eff ex a b
        -> a
        -> IO (Either ex b)
runFlow c cfg store runWrapped flow input =
  try $ runFlowEx c cfg store runWrapped flow input

runSimpleFlow :: forall c a b. (Coordinator c)
        => c
        -> Config c
        -> CS.ContentStore
        -> SimpleFlow a b
        -> a
        -> IO (Either SomeException b)
runSimpleFlow c ccfg store flow input =
  runFlow c ccfg store runNoEffect flow input

-- | Create a full pipeline runner locally. This includes an executor for
--   executing external tasks.
--   This function is specialised to `SimpleFlow` since in cases where
--   a custom term algebra is in use, we assume that probably a centralised
--   coordinator and external runners may be desired as well.
withSimpleLocalRunner :: Path Abs Dir -- ^ Path to content store
                      -> ((SimpleFlow a b -> a -> IO (Either SomeException b))
                           -> IO c)
                      -> IO c
withSimpleLocalRunner storePath action =
  CS.withStore storePath $ \store -> do
    memHook <- createMemoryCoordinator
    withAsync (executeLoop MemoryCoordinator memHook store) $ \_ ->
      action $ runSimpleFlow MemoryCoordinator memHook store
