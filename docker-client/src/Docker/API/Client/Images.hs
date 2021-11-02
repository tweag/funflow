{-# LANGUAGE OverloadedStrings #-}

-- | Helper functions for working with Docker images
module Docker.API.Client.Images
  ( updateImageName,
    updateImageDigest,
    updateImageTag,
    tagImageIfMissing,
  )
where

import Data.Maybe
import qualified Data.Text as T

-- | (name, tag, digest)
type ImageParts = (T.Text, Maybe T.Text, Maybe T.Text)

-- | Split an image name into its individual components
parseImage :: T.Text -> ImageParts
parseImage img =
  let -- (image:..., @digest)
      (nameTag, digest) = T.breakOn "@" img
      (name, tag) = T.breakOn ":" nameTag
      -- Need to remove the prefix symbol if it exists
      formatOptional part = if T.null part then Nothing else Just $ T.tail part
   in (name, formatOptional tag, formatOptional digest)

-- | Adds a character to the head of a text, returns an empty text otherwise
maybeWithSymbol :: T.Text -> (Maybe T.Text -> T.Text)
maybeWithSymbol symbol = maybe "" (symbol <>)

partsToImage :: ImageParts -> T.Text
partsToImage (name, tag, digest) =
  name <> maybeWithSymbol ":" tag <> maybeWithSymbol "@" digest

-- | Adds a `latest` tag to a Docker image if it does not already have
-- a tag or digest specified, otherwise it returns the input image.
tagImageIfMissing :: T.Text -> T.Text
tagImageIfMissing img =
  let (name, tag, digest) = parseImage img
   in if isNothing tag && isNothing digest
        then partsToImage (name, Just "latest", digest)
        else img

-- | Update the name of a docker image, keeping all other fields.
-- For example, you can convert `python:latest` to `perl:latest`.
updateImageName :: T.Text -> T.Text -> T.Text
updateImageName img name =
  let (_, tag, digest) = parseImage img
   in partsToImage (name, tag, digest)

-- | Update the tag of a docker image, keeping all other fields.
-- For example, you can convert `python:latest` to `python:3.7`.
-- Pass a `Nothing` to remove the tag field.
updateImageTag :: T.Text -> Maybe T.Text -> T.Text
updateImageTag img tag =
  let (name, _, digest) = parseImage img
   in partsToImage (name, tag, digest)

-- | Update the tag of a docker image, keeping all other fields.
-- For example, you can convert `python:3.7\@sha256:11111` to `python:3.7\@sha256:22222`.
-- Pass a `Nothing` to remove the digest field.
updateImageDigest :: T.Text -> Maybe T.Text -> T.Text
updateImageDigest img digest =
  let (name, tag, _) = parseImage img
   in partsToImage (name, tag, digest)
