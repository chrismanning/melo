{-# LANGUAGE UndecidableInstances #-}

module Melo.Common.Metadata where

import Control.Applicative
import Control.Concurrent.Classy
import Control.Exception.Safe
import Control.Lens ((^.))
import Control.Monad.Base
import Control.Monad.Identity
import Control.Monad.Parallel (MonadParallel)
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Data.Coerce
import Data.Foldable
import qualified Data.Text as T
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

class Monad m => MetadataService m where
  openMetadataFile :: FilePath -> m (Either F.MetadataException F.MetadataFile)
  openMetadataFileByExt :: FilePath -> m (Either F.MetadataException F.MetadataFile)
  readMetadataFile :: F.MetadataFileId -> FilePath -> m (Either F.MetadataException F.MetadataFile)
  writeMetadataFile :: F.MetadataFile -> FilePath -> m (Either F.MetadataException F.MetadataFile)

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTrans t,
    MetadataService m
  ) =>
  MetadataService (t m)
  where
  openMetadataFile = lift . openMetadataFile
  openMetadataFileByExt = lift . openMetadataFileByExt
  readMetadataFile fid = lift . readMetadataFile fid
  writeMetadataFile f = lift . writeMetadataFile f

newtype MetadataServiceIOT m a = MetadataServiceIOT
  { runMetadataServiceIOT :: m a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadBase b, MonadBaseControl b, MonadConc, MonadCatch, MonadMask, MonadThrow, MonadParallel)
  deriving (MonadTrans, MonadTransControl) via IdentityT

runMetadataServiceIO :: MetadataServiceIOT m a -> m a
runMetadataServiceIO = runMetadataServiceIOT

instance
  ( MonadIO m
  ) =>
  MetadataService (MetadataServiceIOT m)
  where
  openMetadataFile path = liftIO $
    try $ do
      $(logDebugIO) $ "opening metadata file " <> path
      F.openMetadataFile path
  openMetadataFileByExt path = liftIO $
    try $ do
      $(logDebugIO) $ "opening metadata file " <> path
      F.openMetadataFileByExt path
  readMetadataFile mfid@(F.MetadataFileId fid) path = liftIO $ try do
    $(logDebugIO) $ "reading file " <> T.pack path <> " as " <> fid
    F.MetadataFileFactory {readMetadataFile} <- getFactory mfid
    readMetadataFile path
  writeMetadataFile mf path = liftIO $ try do
    F.MetadataFileFactory {writeMetadataFile, readMetadataFile} <- getFactory (mf ^. #fileId)
    writeMetadataFile mf path
    readMetadataFile path

getFactory :: F.MetadataFileId -> IO (F.MetadataFileFactory IO)
getFactory mfid = case F.metadataFileFactoryIO mfid of
  Just fact -> pure fact
  Nothing -> do
    $(logErrorIO) $ T.pack "unknown metadata file id '" <> coerce mfid <> "'"
    throwIO F.UnknownFormat
