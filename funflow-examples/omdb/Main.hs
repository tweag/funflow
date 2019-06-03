{-# LANGUAGE Arrows                #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

import           Control.Arrow
import           Control.Arrow.Async
import           Control.Arrow.Free                          (type (~>), mapA)
import           Control.Exception.Safe                      (SomeException,
                                                              displayException)
import           Control.Funflow
import           Control.Funflow.Cache.TH                    (defaultCacher)
import           Control.Funflow.ContentHashable             (ContentHashable)
import           Control.Funflow.External.Coordinator.Memory
import qualified Control.Funflow.RemoteCache                 as Remote (NoCache(..))
import           Control.Lens                                hiding (Unwrapped,
                                                              Wrapped)
import           Control.Monad                               (join)
import qualified Data.Aeson                                  as Aeson
import           Data.Aeson.Lens
import           Data.Default                                (def)
import qualified Data.Text                                   as T
import           Network.Wreq
import qualified Network.Wreq.Session                        as Sess
import           Options.Generic
import           Path
import           Path.IO

import           Prelude                                     hiding (lookup)

data Opts w = Opts
  { apikey ::  w ::: T.Text <?> "API key for accessing http://www.omdbapi.com"
  , searchTerm :: w ::: T.Text <?> "Term to search for in the movie database."
  } deriving Generic

instance ParseRecord (Opts Wrapped)
deriving instance Show (Opts Unwrapped)

data Ident =
    IdentByTitle T.Text
  | IdentById T.Text
  deriving (Eq,  Show, Generic)

instance Monad m => ContentHashable m Ident

data IMDBQuery i o where
  Search :: Traversal' Aeson.Value T.Text -> IMDBQuery T.Text [T.Text]
  Lookup :: Traversal' Aeson.Value T.Text -> IMDBQuery Ident [T.Text]

type MyFlow = Flow IMDBQuery SomeException

search :: Traversal' Aeson.Value T.Text -> MyFlow T.Text [T.Text]
search l = wrap' props $ Search l
  where
    props = def { cache = $(defaultCacher) }

lookup :: Traversal' Aeson.Value T.Text -> MyFlow Ident [T.Text]
lookup l = wrap' props $ Lookup l
  where
    props = def { cache = $(defaultCacher) }

newtype ImdbQueryRunner = ImdbQueryRunner (forall i o. IMDBQuery i o -> AsyncA IO i o)

imdbQueryRunner :: T.Text -> IO ImdbQueryRunner
imdbQueryRunner apikey' = do
  session <- Sess.newSession
  let
    iqr :: IMDBQuery ~> AsyncA IO
    iqr (Search ls) = AsyncA $ \searchterm -> do
      let opts = defaults & params .~ [ ("apikey", apikey')
                                      , ("s", searchterm)
                                      ]
      r <- Sess.getWith opts session "http://www.omdbapi.com/" >>= asValue
      return $ r ^.. responseBody . ls
    iqr (Lookup ls) = AsyncA $ \ident -> do
      let lkup = case ident of
            IdentByTitle title -> ("t", title)
            IdentById iid      -> ("i", iid)
          opts = defaults & params .~ [ ( "apikey", apikey'), lkup]
      r <- Sess.getWith opts session "http://www.omdbapi.com/" >>= asValue
      return $ r ^.. responseBody . ls
  return $ ImdbQueryRunner iqr

mainFlow :: MyFlow T.Text [T.Text]
mainFlow = proc sTerm -> do
  films <- search (key "Search" . values . key "imdbID" . _String) -< sTerm
  filmsDetail <- mapA (lookup (key "Plot" . _String)) -< IdentById <$> films
  returnA -< join filmsDetail

main :: IO ()
main =  do
  opts <- unwrapRecord "IMDB Query engine"
  ImdbQueryRunner imdbRunner <- imdbQueryRunner (apikey opts)
  memHook <- createMemoryCoordinator
  storeDir <- getXdgDir XdgCache $ Just [reldir|funflow/store|]
  r <- withStore storeDir $ \store ->
    runFlow MemoryCoordinator memHook store Remote.NoCache imdbRunner (Just 1234) mainFlow (searchTerm opts)
  case r of
    Left err ->
      putStrLn $ "FAILED: " <> displayException err
    Right out -> do
      putStrLn "SUCCESS!"
      mapM_ print out
