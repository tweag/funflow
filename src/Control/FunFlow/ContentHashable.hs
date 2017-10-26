module Control.FunFlow.ContentHashable
  ( ContentHashable(..)
  , contentHashUpdate_byteArrayAccess
  , contentHashUpdate_view
  , contentHashUpdate_storable
  , contentHashUpdate_binaryFile

  , FileContent (..)
  , DirectoryContent (..)

  , hashToPath
  , pathToHash

  , SHA256
  , Context
  , Digest
  ) where


import Control.Monad (foldM)
import Crypto.Hash
import Data.ByteArray
import Data.ByteArray.Encoding
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Int
import Data.List (sort)
import Data.Word
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (castPtr)
import Foreign.Storable
import System.Directory
import System.FilePath ((</>))
import System.IO


-- | File path appropriate encoding of a hash
hashToPath :: Digest SHA256 -> FilePath
hashToPath = C8.unpack . convertToBase Base64URLUnpadded


-- | Inverse of 'hashToPath' if given a valid input.
--
-- prop> pathToHash (hashToPath x) = Just x
pathToHash :: FilePath -> Maybe (Digest SHA256)
pathToHash fp = case convertFromBase Base64URLUnpadded (C8.pack fp) of
  Left  _ -> Nothing
  Right x -> digestFromByteString (x :: BS.ByteString)


class ContentHashable a where

  -- | Update a hash context based on the given value.
  --
  -- See 'Crypto.Hash.hashUpdate'.
  contentHashUpdate :: Context SHA256 -> a -> IO (Context SHA256)
  -- XXX: Is this the right default instance?
  -- default contentHashUpdate :: Storable a => Context SHA256 -> a -> IO (Context SHA256)
  -- contentHashUpdate = contentHashUpdate_storable

  -- | Generate hash of the given value.
  --
  -- See 'Crypto.Hash.hash'.
  contentHash :: a -> IO (Digest SHA256)
  contentHash x = hashFinalize <$> contentHashUpdate hashInit x


contentHashUpdate_byteArrayAccess :: ByteArrayAccess a => Context SHA256 -> View a -> IO (Context SHA256)
contentHashUpdate_byteArrayAccess ctx a = pure $ hashUpdate ctx a

contentHashUpdate_view :: ByteArrayAccess a => Context SHA256 -> View a -> IO (Context SHA256)
contentHashUpdate_view ctx a = pure $ hashUpdate ctx a

-- | Update hash context based on binary in memory representation due to 'Foreign.Storable.Storable'.
--
-- XXX: Do we need to worry about endianness?
contentHashUpdate_storable :: Storable a => Context SHA256 -> a -> IO (Context SHA256)
contentHashUpdate_storable ctx a = with a (\p -> pure $! hashUpdate ctx (MemView (castPtr p) (sizeOf a)))

-- | Update hash context based on binary contents of the given file.
contentHashUpdate_binaryFile :: Context SHA256 -> FilePath -> IO (Context SHA256)
contentHashUpdate_binaryFile ctx0 fp = withBinaryFile fp ReadMode $ \h ->
  let go ctx = do
        chunk <- BS.hGetSome h $ hashBlockSize (undefined :: SHA256)
        if BS.null chunk then
          pure ctx
        else
          go $! hashUpdate ctx chunk
  in go ctx0


instance ContentHashable Bool where contentHashUpdate = contentHashUpdate_storable

instance ContentHashable Char where contentHashUpdate = contentHashUpdate_storable

instance ContentHashable Int where contentHashUpdate = contentHashUpdate_storable
instance ContentHashable Int8 where contentHashUpdate = contentHashUpdate_storable
instance ContentHashable Int16 where contentHashUpdate = contentHashUpdate_storable
instance ContentHashable Int32 where contentHashUpdate = contentHashUpdate_storable
instance ContentHashable Int64 where contentHashUpdate = contentHashUpdate_storable

instance ContentHashable Word where contentHashUpdate = contentHashUpdate_storable
instance ContentHashable Word8 where contentHashUpdate = contentHashUpdate_storable
instance ContentHashable Word16 where contentHashUpdate = contentHashUpdate_storable
instance ContentHashable Word32 where contentHashUpdate = contentHashUpdate_storable
instance ContentHashable Word64 where contentHashUpdate = contentHashUpdate_storable

instance ContentHashable Float where contentHashUpdate = contentHashUpdate_storable
instance ContentHashable Double where contentHashUpdate = contentHashUpdate_storable

-- XXX: Define more instances


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
