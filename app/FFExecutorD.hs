{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

import           Control.FunFlow.External.Coordinator.Redis
import           Control.FunFlow.External.Executor
import qualified Data.ByteString                            as BS
import qualified Database.Redis                             as R
import           Options.Generic
import           Path

data Config w = Config {
    storePath :: w ::: FilePath <?> "Path to the root of the content store."
  , redisHost :: w ::: String <?> "Host for the Redis instance."
  , redisPort :: w ::: Int <?> "Port for the Redis instance."
  , redisAuth :: w ::: Maybe BS.ByteString <?> "Password, if needed."
} deriving Generic

instance ParseRecord (Config Wrapped)
deriving instance Show (Config Unwrapped)

main :: IO ()
main = do
  config <- unwrapRecord "ffexecutord"
  let redisConf = R.defaultConnectInfo {
      R.connectHost = redisHost config
    , R.connectPort = R.PortNumber . fromIntegral $ redisPort config
    , R.connectAuth = redisAuth config
    }

  -- XXX: Improve handling of invalid paths.
  storePath' <- parseAbsDir (storePath config)
  executeLoop Redis redisConf storePath'
