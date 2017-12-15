{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
-- | Steps for AWS interaction in FunFlows.
--   Generally, there are two ways of "mixing" AWS interaction into
--   your flow.
--   - Extend your effect type with 'AWSEffect'.
--   - Directly use the AWS Steps in this module.
--
--   However, there are certain steps which cannot be effects, since they
--   interract directly with the store (e.g. by streaming to files).
module Control.FunFlow.AWS.Steps
  ( listBucketContents
  , putInStoreAt
  )
  where

import qualified Aws
import qualified Aws.S3                          as S3
import           Control.Arrow
import           Control.FunFlow.AWS.Effects
import           Control.FunFlow.AWS.S3
import           Control.FunFlow.Base
import           Control.FunFlow.ContentHashable
import           Control.FunFlow.ContentStore    (Content ((:</>)))
import qualified Control.FunFlow.ContentStore    as CS
import           Control.Lens
import           Control.Monad.Trans.Resource    (runResourceT)
import           Data.Conduit                    (($$+-))
import           Data.Conduit.Binary             (sinkFile)
import           Data.Constraint
import           Data.Reflection
import           Network.HTTP.Conduit            (newManager, responseBody,
                                                  tlsManagerSettings)
import           Path
import           Path.IO

-- | Put an item from an S3 bucket directly into the store.
putInStoreAt :: forall objRef eff ex.
                ( ObjectReference objRef
                , (Given Aws.Configuration :=> ContentHashable IO (ObjectInBucket objRef))
                )
             => Aws.Configuration
             -> Flow eff ex (ObjectInBucket objRef, Path Rel File) (CS.Content File)
putInStoreAt conf = give conf $
    (proc (oib, relPath) -> do
      item <- putInStore (\d (oib, p) -> do
        createDirIfMissing True (parent $ d </> p)
        streamToFile (d </> p) oib
        ) -< (oib, relPath)
      returnA -< (item :</> relPath)
    ) \\ (ins :: Given Aws.Configuration :- ContentHashable IO (ObjectInBucket objRef))
  where
    s3cfg = Aws.defServiceConfig :: S3.S3Configuration Aws.NormalQuery
    streamToFile fp oib = do
      mgr <- newManager tlsManagerSettings

      runResourceT $ do
        S3.GetObjectResponse { S3.gorResponse = rsp } <-
          Aws.pureAws conf s3cfg mgr $
            S3.getObject (oib ^. oibBucket) (objectReference $ oib ^. oibObject)

        responseBody rsp $$+- sinkFile (toFilePath fp)

--------------------------------------------------------------------------------
-- Effects turned into steps
--------------------------------------------------------------------------------

listBucketContents :: Aws.Configuration
                   -> Flow eff ex S3.Bucket [ObjectInBucket S3.ObjectInfo]
listBucketContents conf = stepIO $ runAWSEffect conf ListBucketContents
