{-# LANGUAGE Arrows                #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
-- | Steps for AWS interaction in Funflows.
--   Generally, there are two ways of "mixing" AWS interaction into
--   your flow.
--   - Extend your effect type with 'AWSEffect'.
--   - Directly use the AWS Steps in this module.
--
--   However, there are certain steps which cannot be effects, since they
--   interract directly with the store (e.g. by streaming to files).
module Control.Funflow.AWS.Steps
  ( listBucketContents
  , putInStoreAt
  )
  where

import qualified Aws
import qualified Aws.S3                          as S3
import           Control.Arrow
import           Control.Funflow                 (ArrowFlow, putInStore, stepIO)
import           Control.Funflow.AWS.Effects
import           Control.Lens
import           Control.Monad.Trans.Resource    (runResourceT)
import           Data.CAS.ContentHashable
import           Data.CAS.ContentHashable.S3
import           Data.CAS.ContentStore           (Content ((:</>)))
import qualified Data.CAS.ContentStore           as CS
import           Data.Conduit                    (($$+-))
#if MIN_VERSION_conduit(1,3,0)
import           Data.Conduit                    (sealConduitT)
#endif
import           Data.Conduit.Binary             (sinkFile)
import           Data.Constraint
import           Data.Reflection
import           Network.HTTP.Conduit            (newManager, responseBody,
                                                  tlsManagerSettings)
import           Path
import           Path.IO

-- | Put an item from an S3 bucket directly into the store.
putInStoreAt :: forall arr objRef eff ex.
                ( ObjectReference objRef
                , (Given Aws.Configuration :=> ContentHashable IO (ObjectInBucket objRef))
                , ArrowFlow eff ex arr
                )
             => Aws.Configuration
             -> arr (ObjectInBucket objRef, Path Rel File) (CS.Content File)
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
#if MIN_VERSION_conduit(1,3,0)
        sealConduitT (responseBody rsp) $$+- sinkFile (toFilePath fp)
#else
        reponseBody rsp $$+- sinkFile (toFilePath fp)
#endif

--------------------------------------------------------------------------------
-- Effects turned into steps
--------------------------------------------------------------------------------

listBucketContents :: ArrowFlow eff ex arr
                   => Aws.Configuration
                   -> arr S3.Bucket [ObjectInBucket S3.ObjectInfo]
listBucketContents conf = stepIO $ runAWSEffect conf ListBucketContents
