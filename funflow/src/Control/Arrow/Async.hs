{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE StandaloneDeriving    #-}

-- | Asynchronous arrows over monads with MonadUnliftIO, using
--   lifted-async.
module Control.Arrow.Async where

import           Control.Arrow
import           Control.Arrow.Free              (ArrowError (..))
import           Control.Category
import           Control.Exception.Safe          (Exception, MonadCatch)
import qualified Control.Exception.Safe
import           Control.Monad.Trans.Class       (MonadTrans, lift)
import           UnliftIO                        (MonadUnliftIO)
import           UnliftIO.Async
import qualified Data.Profunctor                 as P
import qualified Data.Profunctor.Mapping         as P
import qualified Data.Profunctor.Traversing      as P
import           Prelude                         hiding (id, (.))

newtype AsyncA m a b = AsyncA { runAsyncA :: a -> m b }

deriving via P.WrappedArrow (AsyncA m)
  instance (MonadUnliftIO m) => P.Profunctor (AsyncA m)
deriving via P.WrappedArrow (AsyncA m)
  instance (MonadUnliftIO m) => P.Strong (AsyncA m)
deriving via P.WrappedArrow (AsyncA m)
  instance (MonadUnliftIO m) => P.Choice (AsyncA m)
instance (MonadUnliftIO m) => P.Traversing (AsyncA m) where
  traverse' (AsyncA f) = AsyncA $ mapConcurrently f
instance (MonadUnliftIO m) => P.Closed (AsyncA m) where
  closed = P.closedMapping
instance (MonadUnliftIO m) => P.Mapping (AsyncA m) where
  map' = P.traverseMapping

instance Monad m => Category (AsyncA m) where
  id = AsyncA return
  (AsyncA f) . (AsyncA g) = AsyncA (\b -> g b >>= f)

-- | @since 2.01
instance MonadUnliftIO m => Arrow (AsyncA m) where
  arr f = AsyncA (return . f)
  first (AsyncA f) = AsyncA (\ ~(b,d) -> f b >>= \c -> return (c,d))
  second (AsyncA f) = AsyncA (\ ~(d,b) -> f b >>= \c -> return (d,c))
  (AsyncA f) *** (AsyncA g) = AsyncA $ \ ~(a,b) ->
    withAsync (f a) $ \c ->
      withAsync (g b) $ \d ->
        waitBoth c d

instance MonadUnliftIO m => ArrowChoice (AsyncA m) where
    left f = f +++ arr id
    right f = arr id +++ f
    f +++ g = (f >>> arr Left) ||| (g >>> arr Right)
    AsyncA f ||| AsyncA g = AsyncA (either f g)

instance (Exception ex, MonadUnliftIO m, MonadCatch m)
  => ArrowError ex (AsyncA m) where
  try (AsyncA a) = AsyncA $ Control.Exception.Safe.try . a

-- | Lift an AsyncA through a monad transformer of the underlying monad.
liftAsyncA :: (MonadTrans t, Monad m)
           => AsyncA m i o
           -> AsyncA (t m) i o
liftAsyncA (AsyncA f) = AsyncA $ \i -> lift (f i)
