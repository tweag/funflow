{-# LANGUAGE Strict               #-}
{-# LANGUAGE StrictData           #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Module      : ErrorMonad
Description : This exports a pure error monad and an IO error monad.


In addition to exporting 'IOErrM' and 'ErrM', this module exports
some standard utility functions.
-}


module Data.ErrorMonad
  ( IOErrM
  , ErrM
  , FFCWLExcept (..)
  , runIOErrM
  , injectErrM
  , injectEitherErr
  , injectIOEither
  , injectMaybe
  , injectMaybe'
  , lift
  , throwM
  , guardErrM
  , guardIOErrM
  , modErrM
  , modIOErrM
  , SomeException (..)
  )
where

import Control.Monad.Trans.Except
import Control.Monad.Catch
import Control.Monad.Trans.Class ( lift )

import Text.Parsec ( ParseError )



-- * Error Monad Definitions
---------------------------------------------------------------------


-- | A monad for doing IO with errors
type IOErrM a = ExceptT SomeException IO a

-- | A pure error monad
type ErrM a = Either SomeException a

-- | A custom error/exception type
data FFCWLExcept where
  ErrMsg :: String -> FFCWLExcept
  deriving (Show, Exception)


-- | Executing our IO error monad
runIOErrM :: IOErrM a -> IO (Either SomeException a)
runIOErrM (ExceptT m) = m



-- * Injecting things into our error monads
---------------------------------------------------------------------

-- | Injecting a pure error as an 'IOErrM' error.
injectErrM :: ErrM a -> IOErrM a
injectErrM a = ExceptT $ return a

-- | Inject a typical value that could be an error,
--   i.e., something of type 'Either error a' into 'ErrM'.
injectEitherErr :: Exception e => Either e a -> ErrM a
injectEitherErr = onLeft SomeException

-- | Injecting a typical IO action that could error.
injectIOEither :: Exception e =>
  IO (Either e a) -> IOErrM a
injectIOEither = ExceptT . fmap injectEitherErr

-- | Injecting a 'Maybe value' with a given error for
-- the 'Nothing' case into a 'ErrM'.
injectMaybe :: Exception e => Maybe a -> e -> ErrM a
injectMaybe Nothing e = Left $ SomeException e
injectMaybe (Just a) _ = return a

-- | Just like 'injectMaybe' but for 'IOErrM'.
injectMaybe' :: Exception e => Maybe a -> e -> IOErrM a
injectMaybe' val e = injectErrM $ injectMaybe val e


-- * Guards
---------------------------------------------------------------------


guardErrM :: Exception e => Bool -> e -> ErrM ()
guardErrM True _    = return ()
guardErrM False e = throwM e

guardIOErrM :: Exception e => Bool -> e -> IOErrM ()
guardIOErrM test e = injectErrM $ guardErrM test e


-- * Modify Errors from one domain to another
---------------------------------------------------------------------

type ModError = SomeException -> SomeException

modErrM :: ModError -> ErrM a -> ErrM a
modErrM = onLeft


modIOErrM :: ModError -> IOErrM a -> IOErrM a
modIOErrM f (ExceptT ioEither) =
  ExceptT $ fmap (modErrM f) ioEither




-- # Deriving instances for some errors
---------------------------------------------------------------------


deriving instance Exception ParseError




-- # Internal Library

onLeft :: (a -> b) -> Either a c -> Either b c
onLeft f (Left x) = Left $ f x
onLeft _ (Right x) = Right x



