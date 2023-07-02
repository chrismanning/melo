{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Source.Aggregate where

import Control.Applicative
import Control.Foldl (PrimMonad, impurely, vectorM)
import Control.Monad
import Control.Monad.Base
import Control.Monad.Conc.Class
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Data.Char
import Data.Either.Combinators (mapLeft)
import Data.Range qualified as R
import Data.Text qualified as T
import Data.Time.LocalTime
import Data.Vector qualified as V
import Melo.Common.Config
import Melo.Common.Exception
import Melo.Common.FileSystem
import Melo.Common.Logging
import Melo.Metadata.Aggregate
import Melo.Common.Uri
import Melo.Common.Vector
import Melo.Database.Repo as Repo
import Melo.Format.Metadata (Metadata (..), MetadataFile (..), PictureType(..))
import Melo.Format.Info
import Melo.Library.Release.Aggregate
import Melo.Library.Source.Repo
import Melo.Library.Source.Types
import Streaming.Prelude qualified as S
import System.FilePath as P

class Monad m => SourceAggregate m where
  importSources :: Vector NewImportSource -> m (Vector Source)
  updateSource :: Source -> m (Either SourceError Source)

data SourceError
  = ImportSourceError (Maybe SomeException)
  | UpdateSourceError (Maybe SomeException)
  deriving (Show)
  deriving TextShow via FromStringShow SourceError

instance Exception SourceError

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTrans t,
    SourceAggregate m
  ) =>
  SourceAggregate (t m)
  where
  importSources = lift . importSources
  updateSource = lift . updateSource

newtype SourceAggregateIOT m a = SourceAggregateIOT
  { runSourceAggregateIOT :: m a
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

instance
  ( SourceRepository m,
    ReleaseAggregate m,
    MetadataAggregate m,
    ConfigService m,
    MonadConc m,
    PrimMonad m,
    Logging m
  ) =>
  SourceAggregate (SourceAggregateIOT m)
  where
  importSources ss | null ss = pure mempty
  importSources ss = do
    $(logDebug) $ "Importing " <> showt (V.length ss) <> " sources"
    metadataSources <- S.each ss & S.mapMaybeM transformImportSource & impurely S.foldM_ vectorM
    $(logDebug) $ "Importing " <> showt (V.length metadataSources) <> " metadata sources"
    srcs <- rights . fmap tryFrom <$> insert @SourceEntity (fmap (from @MetadataImportSource) metadataSources)
    let sources = fmap (showt . (.ref)) srcs
    $(logDebugV ['sources]) "Imported sources"
    -- TODO publish sources imported event
    fork $ void $ importReleases srcs
    pure srcs
  updateSource s = do
    e <- runExceptT (writeMetadata >> updateDB)
    void $ importReleases (pure s)
    pure e
    where
      writeMetadata =
        case uriToFilePath s.source of
          Nothing -> pure ()
          Just path -> do
            readMetadataFile s.kind path >>= \case
              Right mf -> do
                config <- getConfigDefault metadataConfigKey
                let mf' = case s.metadata of
                      Just m ->
                        if config.removeOtherTagTypes
                          then mf & #metadata .~  ( mempty & at m.formatId .~ Just m )
                          -- TODO convert metadata to cover other existing types
                          else mf & #metadata . at m.formatId .~ Just m
                      Nothing -> mf
                writeMetadataFile mf' mf'.filePath >>= \case
                  Left e -> do
                    let cause = displayException e
                    $(logWarnV ['cause]) "Failed to write metadata file"
                    throwE (UpdateSourceError $ Just $ SomeException e)
                  Right _ -> pure ()
              Left e -> do
                let cause = displayException e
                $(logWarnV ['cause]) "Failed to read metadata file"
                throwE (UpdateSourceError $ Just $ SomeException e)
      updateDB :: ExceptT SourceError (SourceAggregateIOT m) Source
      updateDB =
        updateSingle @SourceEntity (from s) >>= \case
          Just s' -> ExceptT $ pure $ mapLeft (UpdateSourceError . Just . SomeException) $ tryFrom s'
          Nothing -> throwE $ UpdateSourceError Nothing

transformImportSource :: MetadataAggregate m => NewImportSource -> m (Maybe MetadataImportSource)
transformImportSource s = do
  metadata <- chooseMetadata (getAllMetadata s)
  pure case parseURI (show $ getSourceUri s) of
    Nothing -> Nothing
    Just src -> Just MetadataImportSource
      { audioInfo = getInfo s,
        metadata,
        src,
        metadataFileId = getMetadataFileId s,
        idx = getIdx s,
        range = getRange s,
        collection = getCollectionRef s,
        cover = fmap (const FrontCover) $ lookup FrontCover (getPictures s)
      }
  where
    getIdx (CueFileImportSource _ CueFileSource {idx}) = Just idx
    getIdx _ = Nothing
    getRange (CueFileImportSource _ CueFileSource {range}) = Just range
    getRange (FileSource _ mf) = TimeRange . R.ubi . calendarTimeTime <$> audioLength mf.audioInfo
    getPictures (CueFileImportSource _ cue) = cue.pictures
    getPictures (FileSource _ mf) = mf.pictures

getAllSources :: SourceRepository m => m (Vector Source)
getAllSources = rights <$> fmap tryFrom <$> Repo.getAll @SourceEntity

getSource :: SourceRepository m => SourceRef -> m (Maybe Source)
getSource key = do
  srcs <- Repo.getByKey @SourceEntity (pure key)
  pure $ firstOf traverse $ rights $ tryFrom <$> srcs

getSourcesByUriPrefix ::
  SourceRepository m =>
  URI ->
  m (Vector Source)
getSourcesByUriPrefix prefix = do
  srcs <- getByUriPrefix prefix
  pure (rights $ fmap tryFrom srcs)

length' :: (Foldable f, Num a) => f b -> a
length' = foldl' (const . (+ 1)) 0

getSourceFilePath :: (SourceRepository m) => SourceRef -> m (Maybe FilePath)
getSourceFilePath key = do
  s <- Repo.getSingle @SourceEntity key
  pure (s >>= parseURI . T.unpack . (.source_uri) >>= uriToFilePath)

findCoverImage :: FileSystem m => FilePath -> m (Maybe FilePath)
findCoverImage p = do
  isDir <- doesDirectoryExist p
  if isDir
    then do
      entries <- listDirectory p
      pure $
        find (\e -> P.takeBaseName e =~= "cover" && isImage e) entries
          <|> find (\e -> P.takeBaseName e =~= "front" && isImage e) entries
          <|> find (\e -> P.takeBaseName e =~= "folder" && isImage e) entries
    else pure Nothing
  where
    a =~= b = fmap toLower a == fmap toLower b
    isImage :: FilePath -> Bool
    isImage p =
      let ext = toLower <$> takeExtension p
       in ext == ".jpeg"
            || ext == ".jpg"
            || ext == ".png"
