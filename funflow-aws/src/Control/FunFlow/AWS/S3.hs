{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Control.FunFlow.AWS.S3 where

import qualified Aws
import qualified Aws.S3                          as S3
import           Control.FunFlow.Base
import           Control.FunFlow.ContentHashable
import           Control.Monad.Trans.Resource (runResourceT)
import           Control.Lens
import           Control.Monad                   ((>=>))
import           Data.Aeson
import           Data.Reflection
import qualified Data.Text                       as T
import           GHC.Generics                    (Generic)
import           Network.HTTP.Conduit            (newManager, tlsManagerSettings)

-- | Reference to an object in an S3 bucket
data ObjectInBucket = ObjectInBucket
  { _oibBucket :: T.Text
  , _oibObject :: T.Text
  } deriving (Show, Generic)

makeLenses ''ObjectInBucket

instance FromJSON ObjectInBucket
instance ToJSON ObjectInBucket

-- | An S3 object is hashable whenever we have sufficient configuration to
--   access said object. To deal with this, we use reflection to reify a value
--   (the AWS configuration) into a class constraint.
--   To use this instance, you must reify the value using 'give':
--   @
--     cfg <- Aws.baseConfiguration
--     give cfg $ contentHash s3object
--   @
--
--   Since S3 is already content hashed, we do not need to actually hash the
--   object ourselves. In fact, we avoid fetching the object, and only
--   request the metadata including the content hash.
--   We incorporate the bucket and name into this to give extra guarantees on
--   uniqueness, but we may be better abolishing this to deduplicate files
--   stored in multiple places.
instance (Given Aws.Configuration)
  => ContentHashable IO ObjectInBucket where
  contentHashUpdate ctx a = let
      s3cfg = Aws.defServiceConfig :: S3.S3Configuration Aws.NormalQuery
    in do
      {- Set up a ResourceT region with an available HTTP manager. -}
      mgr <- newManager tlsManagerSettings

      {- Create a request object with S3.getObject and run the request with pureAws. -}
      S3.GetObjectResponse { S3.gorMetadata = md } <- runResourceT $
        Aws.pureAws given s3cfg mgr $
          S3.getObject (a ^. oibBucket) (a ^. oibObject)

      flip contentHashUpdate (a ^. oibBucket)
        >=> flip contentHashUpdate (a ^. oibObject)
        >=> flip contentHashUpdate (S3.omETag md)
          $ ctx
