{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE RankNTypes       #-}
-- | Effects for AWS Funflows.

module Control.Funflow.AWS.Effects where

import qualified Aws
import qualified Aws.S3                          as S3
import           Control.Funflow.AWS.S3
import           Control.Monad.Trans.Resource    (runResourceT)
import           Network.HTTP.Conduit            (newManager,
                                                  tlsManagerSettings)

data AWSEffect i o where
  ListBucketContents :: AWSEffect S3.Bucket [ObjectInBucket S3.ObjectInfo]

-- | Run AWS effects in the IO monad.
--
--   To interpret these in an arrow, you should use the
--   Kleisli arrow or AsyncA arrow over IO.
runAWSEffect :: forall i o.
                Aws.Configuration
             -> AWSEffect i o
             -> (i -> IO o)
runAWSEffect conf ListBucketContents = \bucket -> do
  let s3cfg = Aws.defServiceConfig :: S3.S3Configuration Aws.NormalQuery
  mgr <- newManager tlsManagerSettings

  runResourceT $
    let
      fetchAll mmarker acc = do
        gbr <- Aws.pureAws conf s3cfg mgr
          $ (S3.getBucket bucket) { S3.gbMarker = mmarker}
        case (S3.gbrNextMarker gbr) of
          Nothing -> return $ S3.gbrContents gbr ++ acc
          marker  -> fetchAll marker (S3.gbrContents gbr ++ acc)
    in fmap (ObjectInBucket bucket) <$> fetchAll Nothing []
