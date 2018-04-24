{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UnboxedTuples         #-}

-- | 'ContentHashable' provides a hashing function suitable for use in the
--   Funflow content store.
--
--   This behaves as does a normal hashing function on Haskell types. However,
--   on path types, this instead calculates a hash based on the contents of the
--   file or directory referenced.
--
--   We also export the 'ExternallyAssuredFile' and 'ExternallyAssuredDirectory'
--   types. These instead use the path, file size and modification time to control
--   the hash.
module Control.Funflow.ContentHashable
  ( ContentHash
  , toBytes
  , fromBytes
  , ContentHashable (..)
  , contentHashUpdate_binaryFile
  , contentHashUpdate_byteArray#
  , contentHashUpdate_fingerprint
  , contentHashUpdate_primitive
  , contentHashUpdate_storable

  , FileContent (..)
  , DirectoryContent (..)

  , ExternallyAssuredFile(..)
  , ExternallyAssuredDirectory(..)

  , encodeHash
  , decodeHash
  , hashToPath
  , pathToHash

  , SHA256
  , Context
  , Digest
  ) where


import           Control.Funflow.Orphans          ()
import           Control.Monad                    (foldM, mzero, (>=>))
import           Crypto.Hash                      (Context, Digest, SHA256,
                                                   digestFromByteString,
                                                   hashFinalize, hashInit,
                                                   hashUpdate)
import qualified Data.Aeson                       as Aeson
import qualified Data.Aeson.Types                 as Aeson
import           Data.Bits                        (shiftL)
import           Data.ByteArray                   (Bytes, MemView (MemView),
                                                   allocAndFreeze, convert)
import           Data.ByteArray.Encoding          (Base (Base16),
                                                   convertFromBase,
                                                   convertToBase)
import qualified Data.ByteString                  as BS
import           Data.ByteString.Builder.Extra    (defaultChunkSize)
import qualified Data.ByteString.Char8            as C8
import qualified Data.ByteString.Lazy             as BSL
import           Data.Functor.Contravariant
import qualified Data.HashMap.Lazy                as HashMap
import qualified Data.HashSet                     as HashSet
import           Data.Int
import           Data.List                        (sort)
import           Data.Map                         (Map)
import qualified Data.Map                         as Map
import           Data.Ratio
import           Data.Scientific
import           Data.Store                       (Store (..), peekException)
import qualified Data.Text                        as T
import qualified Data.Text.Array                  as TA
import qualified Data.Text.Encoding               as TE
import qualified Data.Text.Internal               as T
import qualified Data.Text.Lazy                   as TL
import           Data.Time.Clock                  (UTCTime)
import           Data.Time.Clock.POSIX            (utcTimeToPOSIXSeconds)
import           Data.Typeable
import qualified Data.Vector                      as V
import           Data.Word
import qualified Database.SQLite.Simple.FromField as SQL
import qualified Database.SQLite.Simple.ToField   as SQL
import           Foreign.Marshal.Utils            (with)
import           Foreign.Ptr                      (castPtr)
import           Foreign.Storable                 (Storable, sizeOf)
import           GHC.Fingerprint
import           GHC.Generics
import           GHC.Integer.GMP.Internals        (BigNat (..), Integer (..))
import           GHC.Natural                      (Natural (..))
import           GHC.Prim                         (ByteArray#,
                                                   copyByteArrayToAddr#,
                                                   sizeofByteArray#)
import           GHC.Ptr                          (Ptr (Ptr))
import           GHC.Types                        (IO (IO), Int (I#), Word (W#))
import qualified Path
import qualified Path.Internal
import qualified Path.IO
import           System.IO                        (IOMode (ReadMode),
                                                   withBinaryFile)
import           System.IO.Unsafe                 (unsafePerformIO)
import           System.Posix.Files               (fileSize, getFileStatus)


newtype ContentHash = ContentHash { unContentHash :: Digest SHA256 }
  deriving (Eq, Ord, Generic)

instance Aeson.FromJSON ContentHash where
  parseJSON (Aeson.String s)
    | Just h <- decodeHash (TE.encodeUtf8 s) = pure h
    | otherwise = fail "Invalid hash encoding"
  parseJSON invalid
    = Aeson.typeMismatch "ContentHash" invalid
instance Aeson.ToJSON ContentHash where
  toJSON = Aeson.String . TE.decodeUtf8 . encodeHash

instance Show ContentHash where
  showsPrec d h = showParen (d > app_prec)
    $ showString "ContentHash \""
    . (showString $ C8.unpack $ encodeHash h)
    . showString "\""
    where app_prec = 10

instance Store ContentHash where
  size = contramap toBytes size
  peek = fromBytes <$> peek >>= \case
    Nothing -> peekException "Store ContentHash: Illegal digest"
    Just x -> return x
  poke = poke . toBytes

instance SQL.FromField ContentHash where
  fromField f = do
    bs <- SQL.fromField f
    case decodeHash bs of
      Just h  -> pure h
      Nothing -> mzero

instance SQL.ToField ContentHash where
  toField = SQL.toField . encodeHash

toBytes :: ContentHash -> BS.ByteString
toBytes = convert . unContentHash

fromBytes :: BS.ByteString -> Maybe ContentHash
fromBytes bs = ContentHash <$> digestFromByteString bs

hashEncoding :: Base
hashEncoding = Base16

-- | File path appropriate encoding of a hash
encodeHash :: ContentHash -> BS.ByteString
encodeHash = convertToBase hashEncoding . toBytes

-- | Inverse of 'encodeHash' if given a valid input.
--
-- prop> decodeHash (encodeHash x) = Just x
decodeHash :: BS.ByteString -> Maybe ContentHash
decodeHash bs = case convertFromBase hashEncoding bs of
  Left _  -> Nothing
  Right x -> fromBytes x

-- | File path appropriate encoding of a hash
hashToPath :: ContentHash -> Path.Path Path.Rel Path.Dir
hashToPath h =
  case Path.parseRelDir $ C8.unpack $ encodeHash h of
    Nothing -> error
      "[ContentHashable.hashToPath] \
      \Failed to convert hash to directory name"
    Just dir -> dir


-- | Inverse of 'hashToPath' if given a valid input.
--
-- prop> pathToHash (hashToPath x) = Just x
pathToHash :: FilePath -> Maybe ContentHash
pathToHash = decodeHash . C8.pack


class Monad m => ContentHashable m a where

  -- | Update a hash context based on the given value.
  --
  -- See 'Crypto.Hash.hashUpdate'.
  --
  -- XXX: Consider swapping the arguments.
  contentHashUpdate :: Context SHA256 -> a -> m (Context SHA256)

  default contentHashUpdate :: (Generic a, GContentHashable m (Rep a))
    => Context SHA256 -> a -> m (Context SHA256)
  contentHashUpdate ctx a = gContentHashUpdate ctx (from a)

  -- | Generate hash of the given value.
  --
  -- See 'Crypto.Hash.hash'.
  contentHash :: a -> m ContentHash
  contentHash x = ContentHash . hashFinalize <$> contentHashUpdate hashInit x


-- | Update hash context based on binary in memory representation due to 'Foreign.Storable.Storable'.
--
-- XXX: Do we need to worry about endianness?
contentHashUpdate_storable :: (Monad m, Storable a) => Context SHA256 -> a -> m (Context SHA256)
contentHashUpdate_storable ctx a =
  return . unsafePerformIO $ with a (\p -> pure $! hashUpdate ctx (MemView (castPtr p) (sizeOf a)))

-- | Update hash context based on a type's 'GHC.Fingerprint.Type.Fingerprint'.
--
-- The fingerprint is constructed from the library-name, module-name, and name of the type itself.
contentHashUpdate_fingerprint :: (Monad m, Typeable a) => Context SHA256 -> a -> m (Context SHA256)
contentHashUpdate_fingerprint ctx = contentHashUpdate ctx . typeRepFingerprint . typeOf

-- | Update hash context by combining 'contentHashUpdate_fingerprint' and 'contentHashUpdate_storable'.
-- Intended for primitive types like 'Int'.
contentHashUpdate_primitive :: (Monad m, Typeable a, Storable a) => Context SHA256 -> a -> m (Context SHA256)
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

instance Monad m => ContentHashable m Fingerprint where
  contentHashUpdate ctx (Fingerprint a b) = flip contentHashUpdate_storable a >=> flip contentHashUpdate_storable b $ ctx

instance Monad m => ContentHashable m Bool where contentHashUpdate = contentHashUpdate_primitive

instance Monad m => ContentHashable m Char where contentHashUpdate = contentHashUpdate_primitive

instance Monad m => ContentHashable m Int where contentHashUpdate = contentHashUpdate_primitive
instance Monad m => ContentHashable m Int8 where contentHashUpdate = contentHashUpdate_primitive
instance Monad m => ContentHashable m Int16 where contentHashUpdate = contentHashUpdate_primitive
instance Monad m => ContentHashable m Int32 where contentHashUpdate = contentHashUpdate_primitive
instance Monad m => ContentHashable m Int64 where contentHashUpdate = contentHashUpdate_primitive

instance Monad m => ContentHashable m Word where contentHashUpdate = contentHashUpdate_primitive
instance Monad m => ContentHashable m Word8 where contentHashUpdate = contentHashUpdate_primitive
instance Monad m => ContentHashable m Word16 where contentHashUpdate = contentHashUpdate_primitive
instance Monad m => ContentHashable m Word32 where contentHashUpdate = contentHashUpdate_primitive
instance Monad m => ContentHashable m Word64 where contentHashUpdate = contentHashUpdate_primitive

instance Monad m => ContentHashable m Float where contentHashUpdate = contentHashUpdate_primitive
instance Monad m => ContentHashable m Double where contentHashUpdate = contentHashUpdate_primitive

instance (ContentHashable m n, Typeable n) => ContentHashable m (Ratio n) where
  contentHashUpdate ctx x =
    flip contentHashUpdate_fingerprint x
    >=> flip contentHashUpdate (numerator x)
    >=> flip contentHashUpdate (denominator x)
    $ ctx

instance Monad m => ContentHashable m Scientific where
  contentHashUpdate ctx x =
    flip contentHashUpdate_fingerprint x
    >=> flip contentHashUpdate (toRational x)
    $ ctx

instance Monad m => ContentHashable m Integer where
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

instance Monad m => ContentHashable m Natural where
  contentHashUpdate ctx n = ($ ctx) $
    flip contentHashUpdate_fingerprint n >=> case n of
      NatS# w ->
        pure . flip hashUpdate (C8.pack "S") -- tag constructur
        >=> flip contentHashUpdate_storable (W# w) -- hash field
      NatJ# (BN# ba) ->
        pure . flip hashUpdate (C8.pack "L") -- tag constructur
        >=> pure . contentHashUpdate_byteArray# ba 0 (I# (sizeofByteArray# ba)) -- hash field

instance Monad m => ContentHashable m BS.ByteString where
  contentHashUpdate ctx s =
    flip contentHashUpdate_fingerprint s
    >=> pure . flip hashUpdate s $ ctx

instance Monad m => ContentHashable m BSL.ByteString where
  contentHashUpdate ctx s =
    flip contentHashUpdate_fingerprint s
    >=> pure . flip (BSL.foldlChunks hashUpdate) s $ ctx

instance Monad m => ContentHashable m T.Text where
  contentHashUpdate ctx s =
    flip contentHashUpdate_fingerprint s
    >=> pure . flip contentHashUpdate_text s $ ctx

instance Monad m => ContentHashable m TL.Text where
  contentHashUpdate ctx s =
    flip contentHashUpdate_fingerprint s
    >=> pure . flip (TL.foldlChunks contentHashUpdate_text) s $ ctx

instance (Typeable k, Typeable v, ContentHashable m k, ContentHashable m v)
  => ContentHashable m (Map k v) where
  contentHashUpdate ctx m =
    flip contentHashUpdate_fingerprint m
    >=> flip contentHashUpdate (Map.toList m) $ ctx

instance (Typeable k, Typeable v, ContentHashable m k, ContentHashable m v)
  => ContentHashable m (HashMap.HashMap k v) where
  contentHashUpdate ctx m =
    flip contentHashUpdate_fingerprint m
    -- XXX: The order of the list is unspecified.
    >=> flip contentHashUpdate (HashMap.toList m) $ ctx

instance (Typeable v, ContentHashable m v)
  => ContentHashable m (HashSet.HashSet v) where
  contentHashUpdate ctx s =
    flip contentHashUpdate_fingerprint s
    -- XXX: The order of the list is unspecified.
    >=> flip contentHashUpdate (HashSet.toList s) $ ctx

instance ContentHashable m a => ContentHashable m [a] where
  contentHashUpdate = foldM contentHashUpdate

instance ContentHashable m a => ContentHashable m (V.Vector a) where
  contentHashUpdate = V.foldM' contentHashUpdate

instance Monad m => ContentHashable m ()
instance (ContentHashable m a, ContentHashable m b) => ContentHashable m (a, b)
instance (ContentHashable m a, ContentHashable m b, ContentHashable m c) => ContentHashable m (a, b, c)
instance (ContentHashable m a, ContentHashable m b, ContentHashable m c, ContentHashable m d) => ContentHashable m (a, b, c, d)
instance (ContentHashable m a, ContentHashable m b, ContentHashable m c, ContentHashable m d, ContentHashable m e) => ContentHashable m (a, b, c, d, e)
instance (Monad m, ContentHashable m a, ContentHashable m b, ContentHashable m c, ContentHashable m d, ContentHashable m e, ContentHashable m f) => ContentHashable m (a, b, c, d, e, f)
instance (Monad m, ContentHashable m a, ContentHashable m b, ContentHashable m c, ContentHashable m d, ContentHashable m e, ContentHashable m f, ContentHashable m g) => ContentHashable m (a, b, c, d, e, f, g)

instance ContentHashable m a => ContentHashable m (Maybe a)

instance (ContentHashable m a, ContentHashable m b) => ContentHashable m (Either a b)

instance Monad m => ContentHashable m Aeson.Value


class Monad m => GContentHashable m f where
  gContentHashUpdate :: Context SHA256 -> f a -> m (Context SHA256)

instance Monad m => GContentHashable m V1 where
  gContentHashUpdate ctx _ = pure ctx

instance Monad m => GContentHashable m U1 where
  gContentHashUpdate ctx U1 = pure ctx

instance ContentHashable m c => GContentHashable m (K1 i c) where
  gContentHashUpdate ctx x = contentHashUpdate ctx (unK1 x)

instance (Constructor c, GContentHashable m f) => GContentHashable m (C1 c f) where
  gContentHashUpdate ctx0 x = gContentHashUpdate nameCtx (unM1 x)
    where nameCtx = hashUpdate ctx0 $ C8.pack (conName x)

instance (Datatype d, GContentHashable m f) => GContentHashable m (D1 d f) where
  gContentHashUpdate ctx0 x = gContentHashUpdate packageCtx (unM1 x)
    where
      datatypeCtx = hashUpdate ctx0 $ C8.pack (datatypeName x)
      moduleCtx = hashUpdate datatypeCtx $ C8.pack (datatypeName x)
      packageCtx = hashUpdate moduleCtx $ C8.pack (datatypeName x)

instance GContentHashable m f => GContentHashable m (S1 s f) where
  gContentHashUpdate ctx x = gContentHashUpdate ctx (unM1 x)

instance (GContentHashable m a, GContentHashable m b) => GContentHashable m (a :*: b) where
  gContentHashUpdate ctx (x :*: y) = gContentHashUpdate ctx x >>= flip gContentHashUpdate y

instance (GContentHashable m a, GContentHashable m b) => GContentHashable m (a :+: b) where
  gContentHashUpdate ctx (L1 x) = gContentHashUpdate ctx x
  gContentHashUpdate ctx (R1 x) = gContentHashUpdate ctx x

-- XXX: Do we need this?
-- instance GContentHashable (a :.: b) where
--   gContentHashUpdate ctx x = _ (unComp1 x)


instance (Monad m, Typeable b, Typeable t) => ContentHashable m (Path.Path b t) where
  contentHashUpdate ctx p@(Path.Internal.Path fp) =
    flip contentHashUpdate_fingerprint p
    >=> flip contentHashUpdate fp
    $ ctx


-- | Path to a regular file
--
-- Only the file's content and its executable permission is taken into account
-- when generating the content hash. The path itself is ignored.
newtype FileContent = FileContent (Path.Path Path.Abs Path.File)

instance ContentHashable IO FileContent where

  contentHashUpdate ctx (FileContent fp) = do
    exec <- Path.IO.executable <$> Path.IO.getPermissions fp
    ctx' <- if exec then contentHashUpdate ctx () else pure ctx
    contentHashUpdate_binaryFile ctx' (Path.fromAbsFile fp)

-- | Path to a directory
--
-- Only the contents of the directory and their path relative to the directory
-- are taken into account when generating the content hash.
-- The path to the directory is ignored.
newtype DirectoryContent = DirectoryContent (Path.Path Path.Abs Path.Dir)

instance ContentHashable IO DirectoryContent where

  contentHashUpdate ctx0 (DirectoryContent dir0) = do
    (dirs, files) <- Path.IO.listDir dir0
    ctx' <- foldM hashFile ctx0 (sort files)
    foldM hashDir ctx' (sort dirs)
    where
      hashFile ctx fp =
        -- XXX: Do we need to treat symbolic links specially?
        flip contentHashUpdate (Path.filename fp)
        >=> flip contentHashUpdate (FileContent fp)
        $ ctx
      hashDir ctx dir =
        flip contentHashUpdate (Path.dirname dir)
        >=> flip contentHashUpdate (DirectoryContent dir)
        $ ctx

instance Monad m => ContentHashable m UTCTime where
  contentHashUpdate ctx utcTime = let
      secondsSinceEpoch = fromEnum . utcTimeToPOSIXSeconds $ utcTime
    in flip contentHashUpdate_fingerprint utcTime
       >=> flip contentHashUpdate secondsSinceEpoch
         $ ctx

-- | Path to a file to be treated as _externally assured_.
--
--   An externally assured file is handled in a somewhat 'cheating' way by
--   funflow. The 'ContentHashable' instance for such assumes that some external
--   agent guarantees the integrity of the file being referenced. Thus, rather
--   than hashing the file contents, we only consider its (absolute) path, size and
--   modification time, which can be rapidly looked up from filesystem metadata.
--
--   For a similar approach, see the instance for 'ObjectInBucket' in
--   Control.Funflow.AWS.S3, where we exploit the fact that S3 is already
--   content hashed to avoid performing any hashing.
newtype ExternallyAssuredFile = ExternallyAssuredFile (Path.Path Path.Abs Path.File)
  deriving (Generic, Show)

instance Aeson.FromJSON ExternallyAssuredFile
instance Aeson.ToJSON ExternallyAssuredFile
instance Store ExternallyAssuredFile

instance ContentHashable IO ExternallyAssuredFile where
  contentHashUpdate ctx (ExternallyAssuredFile fp) = do
    modTime <- Path.IO.getModificationTime fp
    fSize <- fileSize <$> getFileStatus (Path.toFilePath fp)
    flip contentHashUpdate fp
      >=> flip contentHashUpdate modTime
      >=> flip contentHashUpdate_storable fSize
        $ ctx


-- | Path to a directory to be treated as _externally assured_.
--
--   For an externally assured directory, we _do_ traverse its contents and verify
--   those as we would externally assured files, rather than just relying on the
--   directory path. Doing this traversal is pretty cheap, and it's quite likely
--   for directory contents to be modified without modifying the contents.
newtype ExternallyAssuredDirectory = ExternallyAssuredDirectory (Path.Path Path.Abs Path.Dir)
  deriving (Generic, Show)

instance Aeson.FromJSON ExternallyAssuredDirectory
instance Aeson.ToJSON ExternallyAssuredDirectory
instance Store ExternallyAssuredDirectory

instance ContentHashable IO ExternallyAssuredDirectory where
  contentHashUpdate ctx0 (ExternallyAssuredDirectory dir0) = do
    -- Note that we don't bother looking at the relative directory paths and
    -- including these in the hash. This is because the absolute hash gets
    -- included every time we hash a file.
    (dirs, files) <- Path.IO.listDir dir0
    ctx' <- foldM hashFile ctx0 (sort files)
    foldM hashDir ctx' (sort dirs)
    where
      hashFile ctx fp = contentHashUpdate ctx (ExternallyAssuredFile fp)
      hashDir ctx dir = contentHashUpdate ctx (ExternallyAssuredDirectory dir)
