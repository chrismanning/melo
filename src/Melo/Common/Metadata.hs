{-# LANGUAGE UndecidableInstances #-}

module Melo.Common.Metadata where

import Control.Algebra
import Control.Applicative
import Control.Effect.Exception
import Control.Effect.Lift
import Control.Effect.TH
import Control.Lens ((^.))
import Data.Coerce
import Data.Foldable
import qualified Data.Text as T
import Melo.Common.Effect
import Melo.Common.Logging
import Melo.Format (Metadata (..))
import qualified Melo.Format as F
import qualified Melo.Format.Error as F

chooseMetadata :: [Metadata] -> Maybe Metadata
chooseMetadata ms =
  find (\Metadata {..} -> formatId == F.vorbisCommentsId) ms
    <|> find (\Metadata {..} -> formatId == F.apeV2Id) ms
    <|> find (\Metadata {..} -> formatId == F.apeV1Id) ms
    <|> find (\Metadata {..} -> formatId == F.id3v24Id) ms
    <|> find (\Metadata {..} -> formatId == F.id3v23Id) ms
    <|> find (\Metadata {..} -> formatId == F.id3v1Id) ms

data MetadataService :: Effect where
  OpenMetadataFile :: FilePath -> MetadataService m (Either F.MetadataException F.MetadataFile)
  OpenMetadataFileByExt :: FilePath -> MetadataService m (Either F.MetadataException F.MetadataFile)
  ReadMetadataFile :: F.MetadataFileId -> FilePath -> MetadataService m (Either F.MetadataException F.MetadataFile)
  WriteMetadataFile :: F.MetadataFile -> FilePath -> MetadataService m (Either F.MetadataException F.MetadataFile)

makeSmartConstructors ''MetadataService

newtype MetadataServiceIOC m a = MetadataServiceIOC
  { runMetadataServiceIOC :: m a
  }
  deriving newtype (Functor, Applicative, Monad)

runMetadataServiceIO :: MetadataServiceIOC m a -> m a
runMetadataServiceIO = runMetadataServiceIOC

instance
  ( Has (Lift IO) sig m,
    Has Logging sig m
  ) =>
  Algebra (MetadataService :+: sig) (MetadataServiceIOC m)
  where
  alg _ (L sig) ctx =
    ctx $$!> case sig of
      OpenMetadataFile path -> try do
        $(logDebug) $ "opening metadata file " <> path
        sendIO $ F.openMetadataFile path
      OpenMetadataFileByExt path -> try do
        $(logDebug) $ "opening metadata file " <> path
        sendIO $ F.openMetadataFileByExt path
      ReadMetadataFile mfid@(F.MetadataFileId fid) path -> try $ sendIO do
        $(logDebugIO) $ "reading file " <> T.pack path <> " as " <> fid
        F.MetadataFileFactory {readMetadataFile} <- getFactory mfid
        readMetadataFile path
      WriteMetadataFile mf path -> try $ sendIO do
        F.MetadataFileFactory {writeMetadataFile, readMetadataFile} <- getFactory (mf ^. #fileId)
        writeMetadataFile mf path
        readMetadataFile path
    where
      getFactory :: F.MetadataFileId -> IO (F.MetadataFileFactory IO)
      getFactory mfid = case F.metadataFileFactoryIO mfid of
        Just fact -> pure fact
        Nothing -> do
          $(logErrorIO) $ T.pack "unknown metadata file id '" <> coerce mfid <> "'"
          throwIO F.UnknownFormat
  alg hdl (R other) ctx = MetadataServiceIOC (alg (runMetadataServiceIOC . hdl) other ctx)
