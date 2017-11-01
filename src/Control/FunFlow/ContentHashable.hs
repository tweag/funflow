{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UnboxedTuples        #-}

module Control.FunFlow.ContentHashable
  ( ContentHash
  , ContentHashable(..)
  , contentHashUpdate_binaryFile
  , contentHashUpdate_byteArray#
  , contentHashUpdate_fingerprint
  , contentHashUpdate_primitive
  , contentHashUpdate_storable

  , FileContent (..)
  , DirectoryContent (..)

  , hashToPath
  , pathToHash

  , SHA256
  , Context
  , Digest
  ) where


import           Control.Monad                 (foldM, (>=>))
import           Crypto.Hash                   (Context, Digest, SHA256,
                                                digestFromByteString,
                                                hashFinalize, hashInit,
                                                hashUpdate)
import           Data.Bits                     (shiftL)
import           Data.ByteArray                (Bytes, MemView (MemView),
                                                allocAndFreeze)
import           Data.ByteArray.Encoding       (Base (Base64URLUnpadded),
                                                convertFromBase, convertToBase)
import qualified Data.ByteString               as BS
import           Data.ByteString.Builder.Extra (defaultChunkSize)
import qualified Data.ByteString.Char8         as C8
import qualified Data.ByteString.Lazy          as BSL
import           Data.Int
import           Data.List                     (sort)
import qualified Data.Text                     as T
import qualified Data.Text.Array               as TA
import qualified Data.Text.Internal            as T
import qualified Data.Text.Lazy                as TL
import           Data.Typeable
import           Data.Word
import           Foreign.Marshal.Utils         (with)
import           Foreign.Ptr                   (castPtr)
import           Foreign.Storable
import           GHC.Fingerprint
import           GHC.Generics
import           GHC.Integer.GMP.Internals     (BigNat (..), Integer (..))
import           GHC.Natural                   (Natural (..))
import           GHC.Prim                      (ByteArray#,
                                                copyByteArrayToAddr#,
                                                sizeofByteArray#)
import           GHC.Ptr                       (Ptr (Ptr))
import           GHC.Types                     (IO (IO), Int (I#), Word (W#))
import           System.Directory              (doesFileExist, listDirectory)
import           System.FilePath               ((</>))
import           System.IO                     (IOMode (ReadMode),
                                                withBinaryFile)


newtype ContentHash = ContentHash { unContentHash :: Digest SHA256 }
  deriving (Eq, Show)

-- | File path appropriate encoding of a hash
hashToPath :: ContentHash -> FilePath
hashToPath = C8.unpack . convertToBase Base64URLUnpadded . unContentHash


-- | Inverse of 'hashToPath' if given a valid input.
--
-- prop> pathToHash (hashToPath x) = Just x
pathToHash :: FilePath -> Maybe ContentHash
pathToHash fp = case convertFromBase Base64URLUnpadded (C8.pack fp) of
  Left  _ -> Nothing
  Right x -> ContentHash <$> digestFromByteString (x :: BS.ByteString)


class ContentHashable a where

  -- | Update a hash context based on the given value.
  --
  -- See 'Crypto.Hash.hashUpdate'.
  --
  -- XXX: Consider swapping the arguments.
  contentHashUpdate :: Context SHA256 -> a -> IO (Context SHA256)

  default contentHashUpdate :: (Generic a, GContentHashable (Rep a))
    => Context SHA256 -> a -> IO (Context SHA256)
  contentHashUpdate ctx a = gContentHashUpdate ctx (from a)

  -- | Generate hash of the given value.
  --
  -- See 'Crypto.Hash.hash'.
  contentHash :: a -> IO ContentHash
  contentHash x = ContentHash . hashFinalize <$> contentHashUpdate hashInit x


-- | Update hash context based on binary in memory representation due to 'Foreign.Storable.Storable'.
--
-- XXX: Do we need to worry about endianness?
contentHashUpdate_storable :: Storable a => Context SHA256 -> a -> IO (Context SHA256)
contentHashUpdate_storable ctx a = with a (\p -> pure $! hashUpdate ctx (MemView (castPtr p) (sizeOf a)))

-- | Update hash context based on a type's 'GHC.Fingerprint.Type.Fingerprint'.
--
-- The fingerprint is constructed from the library-name, module-name, and name of the type itself.
contentHashUpdate_fingerprint :: Typeable a => Context SHA256 -> a -> IO (Context SHA256)
contentHashUpdate_fingerprint ctx = contentHashUpdate ctx . typeRepFingerprint . typeOf

-- | Update hash context by combining 'contentHashUpdate_fingerprint' and 'contentHashUpdate_storable'.
-- Intended for primitive types like 'Int'.
contentHashUpdate_primitive :: (Typeable a, Storable a) => Context SHA256 -> a -> IO (Context SHA256)
contentHashUpdate_primitive ctx a =
  flip contentHashUpdate_fingerprint a >=> flip contentHashUpdate_storable a $ ctx

-- | Update hash context based on binary contents of the given file.
contentHashUpdate_binaryFile :: Context SHA256 -> FilePath -> IO (Context SHA256)
contentHashUpdate_binaryFile ctx0 fp = withBinaryFile fp ReadMode $ \h ->
  let go ctx = do
        chunk <- BS.hGetSome h defaultChunkSize
        if BS.null chunk then
          pure ctx
        else
          go $! hashUpdate ctx chunk
  in go ctx0

-- | Update hash context based on 'GHC.Prim.ByteArray#'
-- by copying into a newly allocated 'Data.ByteArray.Bytes'
-- and updating the hash context from there.
--
-- XXX: @'GHC.Prim.byteArrayContents#' :: 'GHC.Prim.ByteArray#' -> 'GHC.Prim.Addr#'@
-- could be used together with 'Data.ByteArray.MemView' instead.
-- However, 'GHC.Prim.byteArrayContents#' explicitly says, that it is only safe to use
-- on a pinned 'GHC.Prim.ByteArray#'.
contentHashUpdate_byteArray# :: ByteArray# -> Int -> Int -> Context SHA256 -> Context SHA256
contentHashUpdate_byteArray# ba (I# off) (I# len) ctx = hashUpdate ctx $
  allocAndFreeze @Bytes (I# len) $ \(Ptr addr) -> IO $ \s ->
    (# copyByteArrayToAddr# ba off addr len s, () #)

-- | Update hash context based on the contents of a strict 'Data.Text.Text'.
contentHashUpdate_text :: Context SHA256 -> T.Text -> Context SHA256
contentHashUpdate_text ctx (T.Text arr off_ len_) =
    contentHashUpdate_byteArray# (TA.aBA arr) off len ctx
    where
      off = off_ `shiftL` 1 -- convert from 'Word16' to 'Word8'
      len = len_ `shiftL` 1 -- convert from 'Word16' to 'Word8'


instance ContentHashable Fingerprint where
  contentHashUpdate ctx (Fingerprint a b) = flip contentHashUpdate_storable a >=> flip contentHashUpdate_storable b $ ctx

instance ContentHashable Bool where contentHashUpdate = contentHashUpdate_primitive

instance ContentHashable Char where contentHashUpdate = contentHashUpdate_primitive

instance ContentHashable Int where contentHashUpdate = contentHashUpdate_primitive
instance ContentHashable Int8 where contentHashUpdate = contentHashUpdate_primitive
instance ContentHashable Int16 where contentHashUpdate = contentHashUpdate_primitive
instance ContentHashable Int32 where contentHashUpdate = contentHashUpdate_primitive
instance ContentHashable Int64 where contentHashUpdate = contentHashUpdate_primitive

instance ContentHashable Word where contentHashUpdate = contentHashUpdate_primitive
instance ContentHashable Word8 where contentHashUpdate = contentHashUpdate_primitive
instance ContentHashable Word16 where contentHashUpdate = contentHashUpdate_primitive
instance ContentHashable Word32 where contentHashUpdate = contentHashUpdate_primitive
instance ContentHashable Word64 where contentHashUpdate = contentHashUpdate_primitive

instance ContentHashable Float where contentHashUpdate = contentHashUpdate_primitive
instance ContentHashable Double where contentHashUpdate = contentHashUpdate_primitive

instance ContentHashable Integer where
  contentHashUpdate ctx n = ($ ctx) $
    flip contentHashUpdate_fingerprint n >=> case n of
      S# i ->
        pure . flip hashUpdate (C8.pack "S") -- tag constructur
        >=> flip contentHashUpdate_storable (I# i) -- hash field
      Jp# (BN# ba) ->
        pure . flip hashUpdate (C8.pack "L") -- tag constructur
        >=> pure . contentHashUpdate_byteArray# ba 0 (I# (sizeofByteArray# ba)) -- hash field
      Jn# (BN# ba) ->
        pure . flip hashUpdate (C8.pack "N") -- tag constructur
        >=> pure . contentHashUpdate_byteArray# ba 0 (I# (sizeofByteArray# ba)) -- hash field

instance ContentHashable Natural where
  contentHashUpdate ctx n = ($ ctx) $
    flip contentHashUpdate_fingerprint n >=> case n of
      NatS# w ->
        pure . flip hashUpdate (C8.pack "S") -- tag constructur
        >=> flip contentHashUpdate_storable (W# w) -- hash field
      NatJ# (BN# ba) ->
        pure . flip hashUpdate (C8.pack "L") -- tag constructur
        >=> pure . contentHashUpdate_byteArray# ba 0 (I# (sizeofByteArray# ba)) -- hash field

instance ContentHashable BS.ByteString where
  contentHashUpdate ctx s =
    flip contentHashUpdate_fingerprint s
    >=> pure . flip hashUpdate s $ ctx

instance ContentHashable BSL.ByteString where
  contentHashUpdate ctx s =
    flip contentHashUpdate_fingerprint s
    >=> pure . flip (BSL.foldlChunks hashUpdate) s $ ctx

instance ContentHashable T.Text where
  contentHashUpdate ctx s =
    flip contentHashUpdate_fingerprint s
    >=> pure . flip contentHashUpdate_text s $ ctx

instance ContentHashable TL.Text where
  contentHashUpdate ctx s =
    flip contentHashUpdate_fingerprint s
    >=> pure . flip (TL.foldlChunks contentHashUpdate_text) s $ ctx

instance ContentHashable a => ContentHashable [a] where
  contentHashUpdate = foldM contentHashUpdate

instance (ContentHashable a, ContentHashable b) => ContentHashable (a, b)
instance (ContentHashable a, ContentHashable b, ContentHashable c) => ContentHashable (a, b, c)
instance (ContentHashable a, ContentHashable b, ContentHashable c, ContentHashable d) => ContentHashable (a, b, c, d)
instance (ContentHashable a, ContentHashable b, ContentHashable c, ContentHashable d, ContentHashable e) => ContentHashable (a, b, c, d, e)
instance (ContentHashable a, ContentHashable b, ContentHashable c, ContentHashable d, ContentHashable e, ContentHashable f) => ContentHashable (a, b, c, d, e, f)
instance (ContentHashable a, ContentHashable b, ContentHashable c, ContentHashable d, ContentHashable e, ContentHashable f, ContentHashable g) => ContentHashable (a, b, c, d, e, f, g)

instance (ContentHashable a, ContentHashable b) => ContentHashable (Either a b)


class GContentHashable f where
  gContentHashUpdate :: Context SHA256 -> f a -> IO (Context SHA256)

instance GContentHashable V1 where
  gContentHashUpdate ctx _ = pure ctx

instance GContentHashable U1 where
  gContentHashUpdate ctx U1 = pure ctx

instance ContentHashable c => GContentHashable (K1 i c) where
  gContentHashUpdate ctx x = contentHashUpdate ctx (unK1 x)

instance (Constructor c, GContentHashable f) => GContentHashable (C1 c f) where
  gContentHashUpdate ctx0 x = gContentHashUpdate nameCtx (unM1 x)
    where nameCtx = hashUpdate ctx0 $ C8.pack (conName x)

instance (Datatype d, GContentHashable f) => GContentHashable (D1 d f) where
  gContentHashUpdate ctx0 x = gContentHashUpdate packageCtx (unM1 x)
    where
      datatypeCtx = hashUpdate ctx0 $ C8.pack (datatypeName x)
      moduleCtx = hashUpdate datatypeCtx $ C8.pack (datatypeName x)
      packageCtx = hashUpdate moduleCtx $ C8.pack (datatypeName x)

instance GContentHashable f => GContentHashable (S1 s f) where
  gContentHashUpdate ctx x = gContentHashUpdate ctx (unM1 x)

instance (GContentHashable a, GContentHashable b) => GContentHashable (a :*: b) where
  gContentHashUpdate ctx (x :*: y) = gContentHashUpdate ctx x >>= flip gContentHashUpdate y

instance (GContentHashable a, GContentHashable b) => GContentHashable (a :+: b) where
  gContentHashUpdate ctx (L1 x) = gContentHashUpdate ctx x
  gContentHashUpdate ctx (R1 x) = gContentHashUpdate ctx x

-- XXX: Do we need this?
-- instance GContentHashable (a :.: b) where
--   gContentHashUpdate ctx x = _ (unComp1 x)


-- | Path to a regular file
--
-- Only the file's content is taken into account when generating the content hash.
-- The path itself is ignored.
newtype FileContent = FileContent FilePath

instance ContentHashable FileContent where

  contentHashUpdate ctx (FileContent fp) = contentHashUpdate_binaryFile ctx fp


-- | Path to a directory
--
-- Only the contents of the directory and their path relative to the directory
-- are taken into account when generating the content hash.
-- The path to the directory is ignored.
newtype DirectoryContent = DirectoryContent FilePath

instance ContentHashable DirectoryContent where

  contentHashUpdate ctx0 (DirectoryContent root) = foldM go ctx0 =<< sort <$> listDirectory root
    where
      go ctx curr = do
        let fp = root </> curr
            ctx' = hashUpdate ctx (C8.pack curr)
        -- XXX: Do we need to treat symbolic links specially?
        isFile <- doesFileExist fp
        if isFile then
          contentHashUpdate ctx' $ FileContent fp
        else do
          contentHashUpdate ctx' $ DirectoryContent fp
