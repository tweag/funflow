{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE AutoDeriveTypeable #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Except
-- Copyright   :  (C) 2013 Ross Paterson
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- This monad transformer extends a monad with the ability throw exceptions.
--
-- A sequence of actions terminates normally, producing a value,
-- only if none of the actions in the sequence throws an exception.
-- If one throws an exception, the rest of the sequence is skipped and
-- the composite action exits with that exception.
--
-- If the value of the exception is not required, the variant in
-- "Control.Monad.Trans.Maybe" may be used instead.
-----------------------------------------------------------------------------

module Control.Monad.Result (
    -- * The ResultT monad transformer
    ResultT(ResultT),
    runResultT,
    mapResultT,

    -- * Exception operations
    throwE,
    catchE
  ) where

import Control.Monad.IO.Class
import Control.Monad.Signatures
import Control.Monad.Trans.Class
import Data.Functor.Classes
import Data.Functor.Identity

import Control.Applicative
import Control.Monad
#if MIN_VERSION_base(4,9,0)
import qualified Control.Monad.Fail as Fail
#endif
import Control.Monad.Fix
#if MIN_VERSION_base(4,4,0)
import Control.Monad.Zip (MonadZip(mzipWith))
#endif
import Data.Foldable (Foldable(foldMap))
import Data.Monoid
import Data.Traversable (Traversable(traverse))
import Control.Concurrent.Async


data Result e a
  = Ready !a
  | Pending !(Async a)
  | Error e
  deriving (Eq, Ord)

instance Functor (Result e) where
    fmap f (Ready x) = Ready $ f x
    fmap f (Error e) = Error e
    fmap f (Pending as) = Pending $ fmap f as

{-
instance Applicative (Result e) where
    pure = Ready
    Error  e <*> _ = Error e
    _ <*> Error  e = Error e
    Ready f <*> r = fmap f r
    Pending f <*> Ready x = Pending $ fmap ($x) f
    Pending f <*> Pending x = Pending $ foo f x

doIO :: IO a -> Async a
doIO = undefined

foo :: Async (a->b) -> Async a -> Async b
foo af ax =  doIO $ do (f,x) <- waitBoth af ax
                       return $ f x
-}

-- | A monad transformer that adds exceptions to other monads.
--
-- @ResultT@ constructs a monad parameterized over two things:
--
-- * e - The exception type.
--
-- * m - The inner monad.
--
-- The 'return' function yields a computation that produces the given
-- value, while @>>=@ sequences two subcomputations, exiting on the
-- first exception.

newtype ResultT e m a = ResultT (m (Result e a))

-- | The inverse of 'ResultT'.
runResultT :: ResultT e m a -> m (Result e a)
runResultT (ResultT m) = m
{-# INLINE runResultT #-}

-- | Map the unwrapped computation using the given function.
--
-- * @'runResultT' ('mapResultT' f m) = f ('runResultT' m)@
mapResultT :: (m (Result e a) -> n (Result e' b))
        -> ResultT e m a
        -> ResultT e' n b
mapResultT f m = ResultT $ f (runResultT m)
{-# INLINE mapResultT #-}

-- | Transform any exceptions thrown by the computation using the
-- given function.
--withResultT :: (Functor m) => (e -> e') -> ResultT e m a -> ResultT e' m a
--withResultT f = mapResultT $ fmap $ either (Left . f) Right
{- INLINE withResultT #-}

instance (Functor m, MonadIO m) => Functor (ResultT e m) where
    fmap f = ResultT . fmap (fmap f) . runResultT
    {-# INLINE fmap #-}

instance (Functor m, MonadIO m) => Applicative (ResultT e m) where
    pure a = ResultT $ return (Ready a)
    {-# INLINE pure #-}
    ResultT f <*> ResultT v = ResultT $ do
        mf <- f
        case mf of
            Error e -> return (Error e)
            Ready k -> do
                mv <- v
                case mv of
                    Error e -> return (Error e)
                    Ready x -> return (Ready (k x))
    {-# INLINEABLE (<*>) #-}
    m *> k = m >>= \_ -> k
    {-# INLINE (*>) #-}


instance (MonadIO m) => Monad (ResultT e m) where
#if !(MIN_VERSION_base(4,8,0))
    return a = ResultT $ return (Ready a)
    {-# INLINE return #-}
#endif
    m >>= k = ResultT $ do
        a <- runResultT m
        case a of
            Error e -> return (Error e)
            Ready x -> runResultT (k x)
            Pending px -> do kx <- liftIO $ fmap k $ wait px
                             runResultT (kx) -- where to call async?
    {-# INLINE (>>=) #-}
    fail = ResultT . fail
    {-# INLINE fail #-}

myWithAsync :: Async a -> (a -> IO b) -> IO (Async b)
myWithAsync as f = async (f =<< wait as)


foo :: (MonadIO m) => (a -> ResultT e m b) -> Async a -> ResultT e m b
foo k px = ResultT $ do
    --return $ Pending $ do
    x <- liftIO $ fmap k $  wait px

    runResultT $  x



-- | Signal an exception value @e@.
--
-- * @'runResultT' ('throwE' e) = 'return' ('Left' e)@
--
-- * @'throwE' e >>= m = 'throwE' e@
throwE :: (Monad m) => e -> ResultT e m a
throwE = ResultT . return . Error
{-# INLINE throwE #-}

-- | Handle an exception.
--
-- * @'catchE' h ('lift' m) = 'lift' m@
--
-- * @'catchE' h ('throwE' e) = h e@
catchE :: (Monad m) =>
    ResultT e m a               -- ^ the inner computation
    -> (e -> ResultT e' m a)    -- ^ a handler for exceptions in the inner
                                -- computation
    -> ResultT e' m a
m `catchE` h = ResultT $ do
    a <- runResultT m
    case a of
        Error  l -> runResultT (h l)
        Ready r -> return (Ready r)
{-# INLINE catchE #-}


