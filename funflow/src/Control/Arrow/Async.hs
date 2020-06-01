{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Asynchronous arrows over monads with MonadUnliftIO, using
--   lifted-async.
module Control.Arrow.Async where

import           Control.Monad.Trans.Class       (MonadTrans, lift)
import Control.Kernmantle.Parallel


type AsyncA = PKleisli

pattern AsyncA :: (a -> m b) -> AsyncA m a b
pattern AsyncA a = PKleisli a

-- | Lift an AsyncA through a monad transformer of the underlying monad.
liftAsyncA :: (MonadTrans t, Monad m)
           => AsyncA m i o
           -> AsyncA (t m) i o
liftAsyncA (AsyncA f) = AsyncA $ \i -> lift (f i)
liftAsyncA _ = error "Should not happen"
