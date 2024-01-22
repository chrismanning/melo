{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Source.Aggregate where

import Control.Applicative
import Control.Foldl (impurely, vectorM)
import Control.Monad.State.Strict
import Data.Char
import Data.HashSet qualified as HashSet
import Data.Range qualified as R
import Data.Text qualified as T
import Data.Time.LocalTime
import Data.Vector qualified as V
import Melo.Common.Config
import Melo.Common.Exception
import Melo.Common.FileSystem
import Melo.Common.Logging
import Melo.Common.Monad
import Melo.Common.Uri
import Melo.Common.Vector
import Melo.Database.Repo as Repo
import Melo.Format.Info
import Melo.Format.Metadata (Metadata (..), MetadataFile (..), PictureType (..))
import Melo.Library.Collection.Types
import Melo.Library.Release.Aggregate
import Melo.Library.Source.Repo
import Melo.Library.Source.Types
import Melo.Metadata.Aggregate
import Streaming.Prelude qualified as S
import System.FilePath as P

class (Monad m) => SourceAggregate m where
  importSources :: Vector NewImportSource -> m (Vector Source)
  updateSource :: Source -> m (Either SourceError Source)

data SourceError
  = ImportSourceError (Maybe SomeException)
  | UpdateSourceError (Maybe SomeException)
  deriving (Show)
  deriving (TextShow) via FromStringShow SourceError

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

instance SourceAggregate (AppM IO IO) where
  importSources ss | null ss = pure mempty
  importSources ss = do
    $(logDebug) $ "Importing " <> showt (V.length ss) <> " sources"
    srcs <-
      S.each ss
        & S.mapMaybeM transformImportSource
        & S.map (from @MetadataImportSource)
        & impurely S.foldM_ vectorM
        >>= insert @SourceEntity
    srcs <-
      S.each srcs
        & S.map (tryInto @Source)
        & S.partitionEithers
        & S.mapM_
          ( \(TryFromException src e) -> do
              let cause = fromMaybe "" $ displayException <$> e
              let source = src.id
              $(logErrorVIO ['cause, 'source]) "Failed to convert source"
          )
        & impurely S.foldM_ vectorM
    let sources = fmap (showt . (.ref)) srcs
    $(logDebugV ['sources]) $ "Imported " <> showt (V.length srcs) <> " sources"
    fork
      do
        collections <- getByKey @CollectionEntity (HashSet.fromList (srcs <&> (.collectionRef) & toList) & toList & V.fromList)
        forM_ collections \collection ->
          when collection.library do
            void $ importReleases srcs
    pure srcs
  updateSource s = do
    e <- try (writeMetadata >> updateDB)
    getSingle @CollectionEntity s.collectionRef >>= \case
      Just collection -> when collection.library do
        void $ importReleases (pure s)
      _ -> pure ()
    pure e
    where
      writeMetadata :: AppM IO IO ()
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
                          then mf & #metadata .~ (mempty & at m.formatId .~ Just m)
                          else -- TODO convert metadata to cover other existing types
                            mf & #metadata . at m.formatId .~ Just m
                      Nothing -> mf
                writeMetadataFile mf' mf'.filePath >>= \case
                  Left e -> do
                    let cause = displayException e
                    $(logWarnV ['cause]) "Failed to write metadata file"
                    throwM (UpdateSourceError $ Just $ SomeException e)
                  Right _ -> pure ()
              Left e -> do
                let cause = displayException e
                $(logWarnV ['cause]) "Failed to read metadata file"
                throwM (UpdateSourceError $ Just $ SomeException e)
      updateDB :: AppM IO IO Source
      updateDB =
        updateSingle @SourceEntity (from s) >>= \case
          Just s' -> throwOnLeft $ first (UpdateSourceError . Just . SomeException) $ tryFrom s'
          Nothing -> throwM $ UpdateSourceError Nothing

transformImportSource :: (MetadataAggregate m) => NewImportSource -> m (Maybe MetadataImportSource)
transformImportSource s = do
  metadata <- chooseMetadata (getAllMetadata s)
  pure case parseURI (show $ getSourceUri s) of
    Nothing -> Nothing
    Just src ->
      Just
        MetadataImportSource
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

getAllSources :: (SourceRepository m) => m (Vector Source)
getAllSources = rights <$> fmap tryFrom <$> Repo.getAll @SourceEntity

getSource :: (SourceRepository m) => SourceRef -> m (Maybe Source)
getSource key = do
  srcs <- Repo.getByKey @SourceEntity (pure key)
  pure $ firstOf traverse $ rights $ tryFrom <$> srcs

getSourcesByUriPrefix ::
  (SourceRepository m) =>
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

findCoverImage :: (FileSystem m) => FilePath -> m (Maybe FilePath)
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
