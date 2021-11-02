{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Funflow.Config where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString.UTF8 as BSU
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Text (Text, unpack)
import Data.Yaml (FromJSON, Object, ParseException, decodeEither', decodeFileThrow, encode, prettyPrintParseException)
import System.Environment (lookupEnv)

-- Re-using Data.Yaml types here since we are parsing config from
-- text anyways.
type ConfigKey = Text

-- Note: EnvConfigMap and ConfigMap have different types since
-- environment variables are expected to only contain a single
-- config value, while file config maps are expected to contain a tree
-- of values (since they are coming from a YAML file). Config coming from
-- a config file needs to get parsed using the default FromJSON parser when
-- initially being read in order to identify which config keys are defined in
-- the file and therefore is of type Object.

type ConfigMap = Object

type EnvConfigMap = HashMap.HashMap Text String

data ExternalConfig = ExternalConfig
  { fileConfig :: ConfigMap,
    envConfig :: EnvConfigMap
    -- Dorran: Disabling this for now until we implement CLI support
    -- cliConfig :: ConfigMap
  }
  deriving (Show)

-- | A value which is intended to be populated using an external source (e.g. a config file)
data Configurable a where
  -- | Define a configurable which will be loaded from a config file using the given key
  ConfigFromFile :: FromJSON a => ConfigKey -> Configurable a
  -- | Define a configurable which will be loaded from the specified environment variable
  ConfigFromEnv :: FromJSON a => ConfigKey -> Configurable a
  -- Dorran: Disabling this for now until we implement CLI support
  -- -- | Define a configurable which will be loaded from the specified command line option
  -- ConfigFromCLI :: FromJSON a => ConfigKey -> Configurable a

  -- | A literal value which does not need to be loaded from an external config source
  Literal :: a -> Configurable a

-- Note: Errors should be raised in the interpreter, so all of this stuff just returns
-- the messages.

-- | Render a Configurable into a Literal value using a set of external configurations, returning
-- an error message if rendering failed.
render :: forall a. Configurable a -> ExternalConfig -> Either String (Configurable a)
render configVal extConfig = case configVal of
  ConfigFromFile key -> appendErrorContext key "config file" $ valueFromObject key $ fileConfig extConfig
  ConfigFromEnv key -> appendErrorContext key "environment variable" $ valueFromStrings key $ envConfig extConfig
  -- ConfigFromCLI key -> appendErrorContext key "CLI args" $ valueFromObject key $ cliConfig extConfig
  Literal _ -> Right configVal
  where
    valueFromStrings :: FromJSON a => Text -> EnvConfigMap -> Either String a
    valueFromStrings k env = case HashMap.lookup k env of
      Nothing -> Left $ "Failed to find key '" ++ unpack k ++ "' in provided config."
      Just v -> case (decodeEither' $ BSU.fromString v) :: Either ParseException a of
        Left parseException -> Left $ prettyPrintParseException parseException
        Right parseResult -> Right parseResult

    valueFromObject :: FromJSON a => Text -> Object -> Either String a
    valueFromObject k obj = case HashMap.lookup k obj of
      Nothing -> Left $ "Failed to find key '" ++ unpack k ++ "' in provided config."
      Just v -> case (decodeEither' $ encode v) :: Either ParseException a of
        Left parseException -> Left $ prettyPrintParseException parseException
        Right parseResult -> Right parseResult

    appendErrorContext :: Text -> String -> Either String a -> Either String (Configurable a)
    appendErrorContext configKey fromConfigName parseResult = case parseResult of
      Left err -> Left $ "Failed to extract configurable " ++ unpack configKey ++ " from " ++ fromConfigName ++ " with error: " ++ err
      Right result -> Right $ Literal result

---------------------------------------------------------------------
-- Functions for gathering and filtering config keys
---------------------------------------------------------------------

-- | Gets the config key for a configurable value, if it exists.
getConfigKey :: Configurable a -> Maybe ConfigKey
getConfigKey conf = case conf of
  ConfigFromFile k -> Just k
  ConfigFromEnv k -> Just k
  -- ConfigFromCLI k -> Just k
  Literal _ -> Nothing

-- | Stores ConfigKey values by their declared sources.
data ConfigKeysBySource = ConfigKeysBySource
  { fileConfigKeys :: HashSet Text,
    envConfigKeys :: HashSet Text
    -- cliConfigKeys :: HashSet Text
  }
  deriving (Show)

-- Note: Making ConfigKeysBySource a Monoid to make them easier to combine
-- in Funflow.Run.

instance Semigroup ConfigKeysBySource where
  (<>) m1 m2 =
    ConfigKeysBySource
      { fileConfigKeys = fileConfigKeys m1 <> fileConfigKeys m2,
        envConfigKeys = envConfigKeys m1 <> envConfigKeys m2
        -- cliConfigKeys = cliConfigKeys m1 <> cliConfigKeys m2
      }

instance Monoid ConfigKeysBySource where
  mempty =
    ConfigKeysBySource
      { fileConfigKeys = HashSet.empty,
        envConfigKeys = HashSet.empty
        -- cliConfigKeys = HashSet.empty
      }

-- | Get the key of a `Configurable` as a `ConfigKeysBySource`.
configKeyBySource :: Configurable a -> ConfigKeysBySource
configKeyBySource conf = case conf of
  ConfigFromFile k ->
    ConfigKeysBySource
      { fileConfigKeys = HashSet.fromList [k],
        envConfigKeys = HashSet.empty
        -- cliConfigKeys = HashSet.empty
      }
  ConfigFromEnv k ->
    ConfigKeysBySource
      { fileConfigKeys = HashSet.empty,
        envConfigKeys = HashSet.fromList [k]
        -- cliConfigKeys = HashSet.empty
      }
  -- ConfigFromCLI k ->
  --   ConfigKeysBySource
  --     { fileConfigKeys = HashSet.empty,
  --       envConfigKeys = HashSet.empty,
  --       cliConfigKeys = HashSet.fromList [k]
  --     }
  _ -> mempty

-- | Get a list of any ConfigKeys which don't exist in their corresponding
-- field in the providedExternalConfig
missing :: ExternalConfig -> ConfigKeysBySource -> [ConfigKey]
missing conf ids =
  let missingFileConfs = filter (not . (flip HashMap.member $ fileConfig conf)) $ HashSet.toList $ fileConfigKeys ids
      missingEnvConfs = filter (not . (flip HashMap.member $ envConfig conf)) $ HashSet.toList $ envConfigKeys ids
   in -- missingCLIConfs = filter (not . (flip HashMap.member $ cliConfig conf)) $ HashSet.toList $ cliConfigKeys ids
      missingFileConfs ++ missingEnvConfs -- ++ missingCLIConfs

---------------------------------------------------------------------
-- IO actions which return configs
---------------------------------------------------------------------

-- | Construct an HashMap containing specified environment variable values.
readEnv :: MonadIO m => ConfigKey -> m (HashMap.HashMap Text String)
readEnv key = do
  val <- liftIO $ lookupEnv $ unpack key
  case val of
    Nothing -> return HashMap.empty
    Just v -> return $ HashMap.fromList [(key, v)]

-- | Convenience function for calling readEnv on a list of ConfigKeys
readEnvs :: MonadIO m => [ConfigKey] -> m (HashMap.HashMap Text String)
readEnvs keys = do
  envVars <- mapM readEnv keys
  return $ mconcat envVars

-- | Construct a HashMap containing the content of a yaml file. Is just an Alias for `decodeFileThrow`.
readYamlFileConfig :: (MonadIO m, FromJSON a) => FilePath -> m a
readYamlFileConfig = decodeFileThrow
