{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
-- | Asynchronous arrows over monads with MonadBaseControl IO, using
--   lifted-async.
module Control.Arrow.Async where

import           Control.Arrow
import           Control.Arrow.Free              (ArrowError (..))
import           Control.Category
import           Control.Concurrent.Async.Lifted
import           Control.Exception.Safe          (Exception, MonadCatch)
import qualified Control.Exception.Safe
import           Control.Monad.Trans.Class       (MonadTrans, lift)
import           Control.Monad.Trans.Control     (MonadBaseControl)
import           Prelude                         hiding (id, (.))

newtype AsyncA m a b = AsyncA { runAsyncA :: a -> m b }

instance Monad m => Category (AsyncA m) where
  id = AsyncA return
  (AsyncA f) . (AsyncA g) = AsyncA (\b -> g b >>= f)

-- | @since 2.01
instance MonadBaseControl IO m => Arrow (AsyncA m) where
  arr f = AsyncA (return . f)
  first (AsyncA f) = AsyncA (\ ~(b,d) -> f b >>= \c -> return (c,d))
  second (AsyncA f) = AsyncA (\ ~(d,b) -> f b >>= \c -> return (d,c))
  (AsyncA f) *** (AsyncA g) = AsyncA $ \ ~(a,b) ->
    withAsync (f a) $ \c ->
      withAsync (g b) $ \d ->
        waitBoth c d

instance MonadBaseControl IO m => ArrowChoice (AsyncA m) where
    left f = f +++ arr id
    right f = arr id +++ f
    f +++ g = (f >>> arr Left) ||| (g >>> arr Right)
    AsyncA f ||| AsyncA g = AsyncA (either f g)

instance (Exception ex, MonadBaseControl IO m, MonadCatch m)
  => ArrowError ex (AsyncA m) where
    AsyncA arr1 `catch` AsyncA arr2 = AsyncA $ \x ->
      arr1 x `Control.Exception.Safe.catch` curry arr2 x

-- | Lift an AsyncA through a monad transformer of the underlying monad.
liftAsyncA :: (MonadTrans t, Monad m)
           => AsyncA m i o
           -> AsyncA (t m) i o
liftAsyncA (AsyncA f) = AsyncA $ \i -> lift (f i)
