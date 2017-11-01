{-# LANGUAGE Arrows                     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

module Control.FunFlow.Exec.Local where

import           Control.Arrow
import           Control.Arrow.Free
import           Control.Exception
import           Control.FunFlow
import           Control.FunFlow.Base
import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Data.ByteString            (ByteString)
import qualified Data.Map.Strict            as M
import           Data.Monoid                ((<>))
import           Data.Store
import qualified Data.Text                  as T


type PureCtx = M.Map T.Text ByteString

newtype LFlowM a = LFLowM { runLFlowM :: ExceptT String (StateT FlowST IO) a }
  deriving (Monad, Applicative, Functor, MonadIO, MonadState FlowST, MonadError String)

type FlowST = (Freshers, PureCtx)

fresh :: LFlowM T.Text
fresh = do
  (fs, ctx) <- get
  let (f,nfs) = popFreshers fs
  put (nfs, ctx)
  return f

lookupSym :: Store a => T.Text -> LFlowM (Maybe a)
lookupSym k = do
  (_, ctx) <- get
  case M.lookup k ctx of
    Nothing -> return Nothing
    Just yv -> case decode yv of
                Right y -> return $ Just y
                Left _  -> return Nothing

putSym :: Store a => T.Text -> a -> LFlowM ()
putSym k x = do
  (fs, ctx) <- get
  put $ (fs, M.insert k (encode x) ctx)


runTillDone :: Flow a b -> a -> IO b
runTillDone f x = go M.empty where
  go st0 = do
    ey <- resumeFlow f x st0
    case ey of
      Right y -> return y
      Left (err, st1) -> do putStrLn $ "Flow failed with "++err
                            go st1

resumeFlow :: forall a b. Flow a b -> a -> PureCtx -> IO (Either (String, PureCtx) b)
resumeFlow f ini ctx = do
  (eres, st) <- runStateT (runExceptT $ runLFlowM $ proceedFlow f ini) (initFreshers, ctx)
  case eres of
    Left err  ->  return $ Left (err, snd st)
    Right res -> return $ Right res


puttingSym :: Store a => T.Text -> a -> LFlowM a
puttingSym n x = putSym n x >> return x


proceedFlow :: Flow a b -> a -> LFlowM b
proceedFlow flow input = runKleisli (evalChoice proceedFlow' flow) input
  where
    proceedFlow' (Named n' f) = Kleisli $ \x -> do
      n <- (n'<>) <$> fresh
      mv <- lookupSym n
      case mv of
        Just y  -> return y
        Nothing -> puttingSym n =<< return (f x)
    proceedFlow' (Step f) = Kleisli $ \x -> do
      n <- fresh
      mv <- lookupSym n
      case mv of
        Just y -> return y
        Nothing -> do
          ey <- liftIO $ fmap Right (f x)
                           `catch`
                             (\e -> return $ Left (show (e::SomeException)))
          case ey of
            Right y  -> puttingSym n y
            Left err -> throwError err
