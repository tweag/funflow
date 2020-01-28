{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
-- | Handling of S3 objects in Funflow.
--
--   This module allows us to fetch objects from S3, taking advantage of S3's
--   support for CAS to avoid the need to calculate our own content hashes.
module Control.Funflow.AWS.S3 where

import qualified Aws
import qualified Aws.S3                          as S3
import           Control.Lens
import           Control.Monad                   ((>=>))
import           Control.Monad.Trans.Resource    (runResourceT)
import           Data.Aeson
import           Data.CAS.ContentHashable
import           Data.Constraint
import           Data.Reflection
import           GHC.Generics                    (Generic)
import           Network.HTTP.Conduit            (newManager,
                                                  tlsManagerSettings)

-- | Reference to an object in an S3 bucket
--
--   Objects can be referenced in a few ways, so this
--   type is parametrised over the object reference.
--   Currently, this is expected to be:
--   - S3.Object (alias for Text)
--   - S3.ObjectInfo
data ObjectInBucket obj = ObjectInBucket
  { _oibBucket :: S3.Bucket
  , _oibObject :: obj
  } deriving (Show, Generic)

makeLenses ''ObjectInBucket

instance FromJSON (ObjectInBucket S3.Object)
instance ToJSON (ObjectInBucket S3.Object)

class ObjectReference a where
  objectReference :: a -> S3.Object

instance ObjectReference S3.Object where
  objectReference = id

instance ObjectReference S3.ObjectInfo where
  objectReference = S3.objectKey

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
  => ContentHashable IO (ObjectInBucket S3.Object) where
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

-- | Reified instance of the implication to allow us to use this as a
--   constraint.
instance (Given Aws.Configuration)
         :=> ContentHashable IO (ObjectInBucket S3.Object) where
  ins = Sub Dict

-- | When we already have `ObjectInfo` (because we have, for example, queried
--   the bucket), we can calculate the 'ContentHash' directly without recourse
--   do S3, because we already know the S3 hash.
instance Monad m => ContentHashable m (ObjectInBucket S3.ObjectInfo) where
  contentHashUpdate ctx a =
    flip contentHashUpdate (a ^. oibBucket)
      >=> flip contentHashUpdate (a ^. oibObject . to S3.objectKey)
      >=> flip contentHashUpdate (a ^. oibObject . to S3.objectETag)
        $ ctx

-- | Reified instance of the implication to allow us to use this as a
--   constraint.
instance (Given Aws.Configuration)
         :=> ContentHashable IO (ObjectInBucket S3.ObjectInfo) where
  ins = Sub Dict
