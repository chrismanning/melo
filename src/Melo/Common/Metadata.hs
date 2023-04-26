{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnboxedTuples #-}

module Melo.Common.Metadata where

import Control.Concurrent.Classy
import Control.Foldl (PrimMonad)
import Control.Lens
import Control.Monad.Base
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Data.Aeson
import Data.Coerce
import Data.Default
import Data.HashMap.Lazy as H
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe
import Data.Text qualified as T
import GHC.Generics
import Melo.Common.Config
import Melo.Common.Exception
import Melo.Common.FileSystem.Watcher
import Melo.Common.Logging
import Melo.Format (Metadata (..))
import Melo.Format qualified as F
import Melo.Format.Error qualified as F

class Monad m => MetadataAggregate m where
  openMetadataFile :: FilePath -> m (Either F.MetadataException F.MetadataFile)
  openMetadataFileByExt :: FilePath -> m (Either F.MetadataException F.MetadataFile)
  readMetadataFile :: F.MetadataFileId -> FilePath -> m (Either F.MetadataException F.MetadataFile)
  writeMetadataFile :: F.MetadataFile -> FilePath -> m (Either F.MetadataException F.MetadataFile)
  chooseMetadata :: [Metadata] -> m (Maybe Metadata)

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTrans t,
    MetadataAggregate m
  ) =>
  MetadataAggregate (t m)
  where
  openMetadataFile = lift . openMetadataFile
  openMetadataFileByExt = lift . openMetadataFileByExt
  readMetadataFile fid = lift . readMetadataFile fid
  writeMetadataFile f = lift . writeMetadataFile f
  chooseMetadata = lift . chooseMetadata

newtype MetadataAggregateIOT m a = MetadataAggregateIOT
  { runMetadataAggregateIOT :: m a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadBase b,
      MonadBaseControl b,
      MonadConc,
      MonadCatch,
      MonadMask,
      MonadThrow,
      PrimMonad
    )
  deriving (MonadTrans, MonadTransControl) via IdentityT

runMetadataAggregateIO :: MetadataAggregateIOT m a -> m a
runMetadataAggregateIO = runMetadataAggregateIOT

instance
  ( MonadIO m,
    ConfigService m,
    FileSystemWatcher m
  ) =>
  MetadataAggregate (MetadataAggregateIOT m)
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
    F.MetadataFileFactory {readMetadataFile} <- getFactoryIO mfid
    readMetadataFile path
  writeMetadataFile mf path = lockPathsDuring (path :| []) $ liftIO $ try do
    F.MetadataFileFactory {writeMetadataFile, readMetadataFile} <- getFactoryIO mf.fileId
    writeMetadataFile mf path
    readMetadataFile path
  chooseMetadata ms = do
    config <- getConfigDefault metadataConfigKey
    let metadata = H.fromList $ fmap (\m -> (m.formatId, m)) ms
    case catMaybes (config.tagPreference <&> \mid -> metadata ^. at mid) of
      (m : _) -> pure $ Just m
      [] -> pure Nothing

getFactoryIO :: F.MetadataFileId -> IO (F.MetadataFileFactory IO)
getFactoryIO mfid = case F.metadataFileFactoryIO mfid of
  Just fact -> pure fact
  Nothing -> do
    $(logErrorIO) $ T.pack "unknown metadata file id '" <> coerce mfid <> "'"
    throwIO F.UnknownFormat

data MetadataConfig = MetadataConfig
  { removeOtherTagTypes :: Bool
  , tagPreference :: [F.MetadataId]
  }
  deriving (Show, Eq, Generic)

instance Default MetadataConfig where
  def =
    MetadataConfig
      { removeOtherTagTypes = False
      , tagPreference = [
          F.MetadataId "CUE",
          F.vorbisCommentsId,
          F.apeV2Id,
          F.apeV1Id,
          F.id3v24Id,
          F.id3v23Id,
          F.riffId,
          F.id3v1Id
        ]
      }

instance FromJSON MetadataConfig

instance ToJSON MetadataConfig

deriving newtype instance FromJSON F.MetadataId

deriving newtype instance ToJSON F.MetadataId

metadataConfigKey :: ConfigKey MetadataConfig
metadataConfigKey = ConfigKey "metadata"

initMetadataConfig :: ConfigService m => m ()
initMetadataConfig = setConfig metadataConfigKey def
