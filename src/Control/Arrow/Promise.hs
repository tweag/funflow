{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}


module Control.Arrow.Promise
  ( Info (..)
  , Promise (..)
  , PromiseT (..)
  , PromiseA (..)
  , promise
  , waitPromise
  , iterPromise
  , stepPromise
  , cancelPromise
  , apPromise
  , multPromise
  , kleisli
  ) where


import           Control.Arrow
import           Control.Arrow.Free       (ArrowError (..))
import           Control.Category
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Control.Monad.Catch      (MonadCatch)
import qualified Control.Monad.Catch      as Monad.Catch
import           Control.Monad.IO.Class
import           Data.Monoid              ((<>))
import           Data.Sequence            (Seq)
import qualified Data.Sequence            as Seq
import           Prelude                  hiding ((.), id)


data Info i
  = I i
  -- ^ Info about one pending task
  | Info i :|!|: Info i
  -- ^ Info about two parallel pending tasks
  | Info i :-!>: Info i
  -- ^ Info about two sequential pending tasks
  deriving (Show)

-- | Captures the result of a collection of computations that is either
-- completed ('Ready' or 'Failed'), or ongoing in parallel ('Pending').
data Promise i a
  -- | The computations produced a result.
  = Ready a
  -- | The computations failed with the given errors.
  --
  -- XXX: Consider dropping that constructor and using the 'Async' to carry
  -- exceptions. That might allow for a cleaner 'ArrowError' instance and for
  -- cleaner lifting into the flow itself in the future.
  | Failed (Seq SomeException)
  -- | The sequence of computations is ongoing asynchronously.
  -- The first field holds information about the pending computations.
  -- The second field holds an 'Control.Concurrent.Async.Async'
  -- that will complete when the next computation completes
  -- and return the result, error, or remaining computations.
  | Pending (Info i) (Async (Promise i a))
  deriving Functor


newtype PromiseT i m a = PromiseT { unPromiseT :: m (Promise i a) }
  deriving Functor

instance (Monoid i, MonadIO m) => Applicative (PromiseT i m) where
  pure = PromiseT . pure . Ready
  PromiseT mf <*> PromiseT mx = PromiseT $ do
    pf <- mf
    px <- mx
    liftIO $ apPromise pf px


-- | Wraps a computation that takes a promise
-- and returns a promise in a monadic context.
newtype PromiseA i m a b =
  PromiseA { unPromiseA :: Promise i a -> PromiseT i m b }

instance Monad m => Category (PromiseA i m) where
  id = PromiseA $ PromiseT . pure
  PromiseA f . PromiseA g = PromiseA $ PromiseT .
    (unPromiseT . f <=< unPromiseT . g)

instance (Monoid i, MonadIO m) => Arrow (PromiseA i m) where
  arr f = PromiseA $ PromiseT . pure . fmap f
  PromiseA f *** PromiseA g = PromiseA $ \pab -> PromiseT $ do
    let pa = fst <$> pab
        pb = snd <$> pab
    pc <- unPromiseT $ f pa
    pd <- unPromiseT $ g pb
    liftIO $ multPromise pc pd

instance (Monoid i) => ArrowChoice (PromiseA i IO) where
  f +++ g = PromiseA $ \case
    Ready (Left a) -> PromiseT $ f' a
    Ready (Right b) -> PromiseT $ g' b
    Failed err -> PromiseT $ return $ Failed err
    Pending info_ab async_ab -> PromiseT $ do
      async_cd <- async $ waitCatch async_ab >>= \case
        Left err -> return $ Failed (Seq.singleton err)
        Right pab' -> unPromiseT $ unPromiseA (f +++ g) pab'
      return $ Pending info_ab async_cd
    where
      f' = fmap (fmap Left) . unPromiseT . unPromiseA f . Ready
      g' = fmap (fmap Right) . unPromiseT . unPromiseA g . Ready
  f ||| g = PromiseA $ \case
    Ready (Left a) -> PromiseT $ f' a
    Ready (Right b) -> PromiseT $ g' b
    Failed err -> PromiseT $ return $ Failed err
    Pending info_ab async_ab -> PromiseT $ do
      async_c <- async $ waitCatch async_ab >>= \case
        Left err -> return $ Failed (Seq.singleton err)
        Right pab' -> unPromiseT $ unPromiseA (f ||| g) pab'
      return $ Pending info_ab async_c
    where
      f' = unPromiseT . unPromiseA f . Ready
      g' = unPromiseT . unPromiseA g . Ready

instance (Exception ex, MonadCatch m) => ArrowError ex (PromiseA i m) where
  -- XXX: Potentially dodgy instance.
  -- Errors might be hiding in the outgoing promise's 'Failed' constructor.
  f `catch` h = PromiseA $ \pe -> PromiseT $ do
    (unPromiseT $ unPromiseA f pe)
    `Monad.Catch.catch`
    (\ex -> unPromiseT $ unPromiseA h $ (, ex) <$> pe)


promise :: i -> IO (Promise i a) -> IO (Promise i a)
promise i m = Pending (I i) <$> async m

waitPromise :: Promise i a -> IO (Either (Seq SomeException) a)
waitPromise = iterPromise (const (return ()))

iterPromise :: (Info i -> IO ()) -> Promise i a -> IO (Either (Seq SomeException) a)
iterPromise f = go
  where
    go = \case
      Ready a -> return $ Right a
      Failed err -> return $ Left err
      p@(Pending info _) -> f info >> stepPromise p >>= go

stepPromise :: Promise i a -> IO (Promise i a)
stepPromise = \case
  Ready a -> return $ Ready a
  Failed err -> return $ Failed err
  Pending _info async_ -> waitCatch async_ >>= \case
    Left err -> return $ Failed (Seq.singleton err)
    Right p -> return p


cancelPromise :: Promise i a -> IO ()
cancelPromise = \case
  Ready _ -> return ()
  Failed _ -> return ()
  Pending _ async_ -> cancel async_


apPromise :: Monoid i => Promise i (a -> b) -> Promise i a -> IO (Promise i b)
apPromise pf px = case (pf, px) of
  (Ready f, _) -> return $ f <$> px
  (_, Ready x) -> return $ ($ x) <$> pf
  (Failed err_f, Failed err_x) -> return $ Failed (err_f <> err_x)
  (Failed err_f, _) -> do
    cancelPromise px
    return $ Failed err_f
  (_, Failed err_x) -> do
    cancelPromise pf
    return $ Failed err_x
  (Pending info_f async_f, Pending info_x async_x) -> do
    let info_y = info_f :|!|: info_x
    async_y <- async $ waitEitherCatch async_f async_x >>= \case
      Left (Right pf') -> apPromise pf' px
      Right (Right px') -> apPromise pf px'
      Left (Left err_f) -> do
        cancelPromise px
        return $ Failed (Seq.singleton err_f)
      Right (Left err_x) -> do
        cancelPromise pf
        return $ Failed (Seq.singleton err_x)
    return $ Pending info_y async_y

multPromise :: Monoid i => Promise i a -> Promise i b -> IO (Promise i (a, b))
multPromise pa pb = case (pa, pb) of
  (Ready a, _) -> return $ (a, ) <$> pb
  (_, Ready b) -> return $ (, b) <$> pa
  (Failed err_f, Failed err_x) -> return $ Failed (err_f <> err_x)
  (Failed err_f, _) -> do
    cancelPromise pb
    return $ Failed err_f
  (_, Failed err_x) -> do
    cancelPromise pa
    return $ Failed err_x
  (Pending info_f async_f, Pending info_x async_x) -> do
    let info_y = info_f :|!|: info_x
    async_y <- async $ waitEitherCatch async_f async_x >>= \case
      Left (Right pf') -> multPromise pf' pb
      Right (Right px') -> multPromise pa px'
      Left (Left err_f) -> do
        cancelPromise pb
        return $ Failed (Seq.singleton err_f)
      Right (Left err_x) -> do
        cancelPromise pa
        return $ Failed (Seq.singleton err_x)
    return $ Pending info_y async_y

-- XXX: Use lifted async
--kleisli :: (Monoid i, MonadIO m)
--  => i -> (a -> PromiseT i m b) -> PromiseA i m a b
kleisli :: i -> (a -> PromiseT i IO b) -> PromiseA i IO a b
kleisli info_k k = PromiseA $ \case
  Ready a -> k a
  Failed err -> PromiseT $ pure $ Failed err
  Pending info_a async_a -> PromiseT $ do
    async_b <- liftIO $ async $ waitCatch async_a >>= \case
      Left err -> return $ Failed (Seq.singleton err)
      Right pa' -> unPromiseT $ unPromiseA (kleisli info_k k) pa'
    return $ Pending (info_a :-!>: I info_k) async_b
