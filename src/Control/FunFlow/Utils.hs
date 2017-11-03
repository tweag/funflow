module Control.FunFlow.Utils where

import Control.Monad.Except
import Data.Store
import Data.ByteString (ByteString)
import Lens.Micro.Platform

mdecode :: Store a => Maybe ByteString -> Either String a
mdecode (Nothing) = Left "no value"
mdecode (Just bs) = over _Left show $ decode bs

catching :: MonadError s m => m a -> m (Either s a)
catching mx =(fmap Right mx) `catchError` (\e -> return $ Left e)

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing _ = return ()
whenJust (Just x) f = f x

whenRight :: Monad m => Either b a -> (a -> m ()) -> m ()
whenRight (Left _) _ = return ()
whenRight (Right x) f = f x