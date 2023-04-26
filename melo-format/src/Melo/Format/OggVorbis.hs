module Melo.Format.OggVorbis
  ( OggVorbis (..),
    hReadOggVorbis,
    oggVorbisFileId,
    oggVorbis,
    readOggVorbisFile,
    writeOggVorbisFile,
  )
where

import Control.Exception.Safe
import Data.Binary
import Data.Binary.Put
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import Data.Either (isRight)
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as H
import Melo.Format.Internal.Binary (bdecodeOrThrowIO)
import Melo.Format.Internal.Info
import Melo.Format.Internal.Metadata
import Melo.Format.Ogg
import Melo.Format.Vorbis as V
import Streaming.Binary qualified as S
import Streaming.ByteString qualified as S
import System.Directory
import System.FilePath
import System.IO

oggVorbisFileId :: MetadataFileId
oggVorbisFileId = MetadataFileId "OggVorbis"

oggVorbis :: MetadataFileFactory IO
oggVorbis =
  MetadataFileFactory
    { priority = 100,
      fileId = oggVorbisFileId,
      detectFile = \p -> withBinaryFile p ReadMode $ \h -> do
        hSeek h AbsoluteSeek 0
        (_, _i, e) <- S.decode @(OggPage Header) (S.hGetContents h)
        hSeek h AbsoluteSeek 0
        pure (isRight e),
      readMetadataFile = readOggVorbisFile,
      writeMetadataFile = writeOggVorbisFile
    }

readOggVorbisFile :: FilePath -> IO MetadataFile
readOggVorbisFile p = do
  ogg <- withBinaryFile p ReadMode hReadOggVorbis
  pure
    MetadataFile
      { audioInfo = info ogg,
        fileId = oggVorbisFileId,
        metadata = oggVorbisMetadata ogg,
        filePath = p,
        pictures = []
      }

oggVorbisMetadata :: OggVorbis -> H.HashMap MetadataId Metadata
oggVorbisMetadata (OggVorbis _ (OggPage _ (FramedVorbisComments vc) _)) =
  let fmt = metadataFormat @VorbisComments
   in H.singleton fmt.formatId (extractMetadata vc)

writeOggVorbisFile :: MetadataFile -> FilePath -> IO ()
writeOggVorbisFile f newpath = do
  oldpath <- canonicalizePath f.filePath
  newpath <- canonicalizePath newpath
  if oldpath == newpath
    then do
      (tmpfile, h) <- openBinaryTempFile (takeDirectory newpath) (takeBaseName newpath <> ".tmp")
      hClose h
      handleAny (\e -> removeFile tmpfile >> throw e) do
        writeOggVorbisFile' oldpath tmpfile
        copyPermissions oldpath tmpfile
        renameFile tmpfile newpath
    else writeOggVorbisFile' oldpath newpath
  where
    writeOggVorbisFile' oldpath newpath = do
      !(ov :: OggVorbis) <-
        updateOggVorbisWith (H.elems f.metadata)
          <$> withBinaryFile oldpath ReadMode hReadOggVorbis
      !audioData <- withBinaryFile oldpath ReadMode $ \h -> do
        hSeek h SeekFromEnd 0
        end <- hTell h
        hSeek h AbsoluteSeek (oggVorbisSize ov)
        BS.hGet h $ fromInteger (end - oggVorbisSize ov)
      withBinaryFile newpath WriteMode $ \h -> do
        hWriteOggVorbis h ov
        BS.hPut h audioData

updateOggVorbisWith :: [Metadata] -> OggVorbis -> OggVorbis
updateOggVorbisWith ms ogg@(OggVorbis identPage (OggPage header (FramedVorbisComments vcs) s)) =
  case F.find (\m -> m.formatId == vorbisCommentsId) ms of
    Just Metadata {tags} ->
      OggVorbis identPage (OggPage header (FramedVorbisComments (replaceWithTags vcs tags)) s)
    Nothing -> ogg

data OggVorbis = OggVorbis !(OggPage Identification) !(OggPage FramedVorbisComments)
  deriving (Eq, Show)

oggVorbisSize :: OggVorbis -> Integer
oggVorbisSize ogg = fromIntegral $ L.length $ runPut (put ogg)

instance Binary OggVorbis where
  get = OggVorbis <$> getIdent <*> getComments
    where
      getIdent =
        get >>= \case
          OggPage header (IdentificationHeader ident) s -> pure $ OggPage header ident s
          _otherPage -> fail "expected vorbis identification header"
      getComments =
        get >>= \case
          OggPage header (CommentsHeader vc) s -> pure $ OggPage header vc s
          _otherPage -> fail "expected vorbis comments header"
  put (OggVorbis ident fvc) =
    put (IdentificationHeader <$> ident) >> put (CommentsHeader <$> fvc)

instance InfoReader OggVorbis where
  info (OggVorbis (OggPage _ ident _) _) =
    Info
      { sampleRate = SampleRate $ fromIntegral $ V.sampleRate ident,
        channels = case V.channels ident of
          1 -> Mono
          2 -> Stereo
          _ -> MultiChannel ChannelMask,
        totalSamples = Nothing, -- TODO ogg vorbis total samples
        bitsPerSample = Nothing,
        quality = Nothing -- TODO ogg vorbis quality
      }

hReadOggVorbis :: Handle -> IO OggVorbis
hReadOggVorbis h = do
  hSeek h AbsoluteSeek 0
  bdecodeOrThrowIO $ S.hGetContents h

hWriteOggVorbis :: Handle -> OggVorbis -> IO ()
hWriteOggVorbis h ogg = do
  let buf = L.toStrict $ runPut (put ogg)
  BS.hPut h buf
