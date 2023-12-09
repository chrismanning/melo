{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Source.Transform where

import Control.Applicative
import Control.Concurrent.Classy
import Control.Foldl qualified as Fold
import Control.Lens (to)
import Control.Monad.State.Class
import Control.Monad.Trans.State.Strict (StateT, evalStateT)
import Data.Attoparsec.Text qualified as P
import Data.Attoparsec.Text ((<?>))
import Data.Char
import Data.Default
import Data.HashMap.Strict qualified as HashMap
import Data.Map.Strict (Map)
import Data.Text qualified as T
import Data.Vector qualified as V
import Melo.Common.Exception as E
import Melo.Common.FileSystem as FS
import Melo.Common.FileSystem.Watcher
import Melo.Common.Logging
import Melo.Metadata.Aggregate
import Melo.Common.Monad
import Melo.Common.Uri
import Melo.Common.Uuid
import Melo.Database.Repo qualified as Repo
import Melo.Format qualified as F
import Melo.Format.Error qualified as F
import Melo.Format.Internal.Metadata (Metadata (..))
import Melo.Format.Mapping qualified as M
import Melo.Library.Artist.Aggregate as Artist
import Melo.Library.Artist.Name.Repo
import Melo.Library.Artist.Name.Types
import Melo.Library.Artist.Repo as Artist
import Melo.Library.Artist.Types
import Melo.Library.Collection.Repo as Collection
import Melo.Library.Collection.Types
import Melo.Library.Release.Aggregate as Release
import Melo.Library.Release.ArtistName.Repo as ReleaseArtist
import Melo.Library.Release.ArtistName.Types
import Melo.Library.Release.Repo as Release
import Melo.Library.Release.Types
import Melo.Library.Source.Aggregate
import Melo.Library.Source.MultiTrack
import Melo.Library.Source.Repo as Src
import Melo.Library.Source.Types
import Melo.Library.Track.Aggregate
import Melo.Library.Track.ArtistName.Repo as TrackArtist
import Melo.Library.Track.ArtistName.Types
import Melo.Library.Track.Repo as Track
import Melo.Library.Track.Types
import Melo.Lookup.Covers qualified as Covers
import Melo.Lookup.MusicBrainz as MB
import Melo.Metadata.Mapping.Aggregate
import Melo.Metadata.Mapping.Repo
import Melo.Metadata.Mapping.Types
import System.FilePath
import Text.Printf

type Transform m = Source -> m (Either TransformationError Source)

data TransformAction =
    Move { destinationCollection :: Maybe CollectionRef, outputPathPattern :: NonEmpty SourcePathPattern }
  | Copy { destinationCollection :: Maybe CollectionRef, outputPathPattern :: NonEmpty SourcePathPattern }
  | ExtractEmbeddedImage URI
  | EmbedImage URI
  | CopyCoverImage URI
  | SplitMultiTrackFile { destinationCollection :: Maybe CollectionRef, outputPathPattern :: NonEmpty SourcePathPattern }
  | RemoveOtherFiles
  | MusicBrainzLookup
  | ConvertEncoding Text
  | EditMetadata (Vector MetadataTransformation)
  | ConvertMetadataFormat F.MetadataId
  | ConvertFileFormat F.MetadataFileId
  deriving (Generic)
  deriving TextShow via FromGeneric TransformAction
  deriving (FromJSON, ToJSON) via CustomJSON JSONOptions TransformAction

evalTransformActions :: MonadSourceTransform m => Vector TransformAction -> Source -> m (Either TransformationError Source)
evalTransformActions ts s = lockSource s $
  foldl' (\b a -> b >=/> evalTransformAction a) (pure . Right) (optimiseTransformActions ts) s
  where
    lockSource s m = case uriToFilePath s.source of
      Just p -> lockPathsDuring (p :| []) m
      Nothing -> m

optimiseTransformActions :: Vector TransformAction -> Vector TransformAction
optimiseTransformActions ts = V.fromList $ impl $ V.toList ts
  where
    impl (EditMetadata a : EditMetadata b : c) = EditMetadata (a <> b) : impl c
    impl (a : b) = a : impl b
    impl [] = []

(>=/>) :: Monad m => Transform m -> Transform m -> Transform m
f >=/> g = \x ->
  f x >>= \case
    Left e -> pure $ Left e
    Right s -> g s

evalTransformAction :: MonadSourceTransform m => TransformAction -> Transform m
evalTransformAction (Move ref patterns) src = first from <$> moveSourceWithPattern ref patterns src
evalTransformAction (SplitMultiTrackFile ref patterns) src = first from <$> extractTrack ref patterns src
evalTransformAction (EditMetadata metadataTransformations) src = first from <$> editMetadata metadataTransformations src
evalTransformAction MusicBrainzLookup src = musicBrainzLookup src
evalTransformAction (CopyCoverImage coverUrl) src = copyCoverImage coverUrl src
evalTransformAction (ConvertMetadataFormat mid) src = convertMetadataFormat mid src
evalTransformAction t _ = pure $ Left (UnsupportedTransform (showt t))

type MonadSourceTransform m =
  ( CollectionRepository m,
    FileSystem m,
    FileSystemWatchLocks m,
    Logging m,
    MetadataAggregate m,
    Monad m,
    MonadCatch m,
    MonadConc m,
    PrimMonad m,
    MultiTrack m,
    MusicBrainzService m,
    SourceAggregate m,
    ReleaseAggregate m,
    ArtistAggregate m,
    TrackAggregate m,
    SourceRepository m,
    TagMappingRepository m,
    TagMappingAggregate m,
    Covers.CoverService m,
    ArtistRepository m
  )

previewTransformation ::
  MonadSourceTransform m =>
  Transform
    ( TransformPreviewT
        (VirtualArtistRepoT (StateT VirtualEntities m))
    ) ->
  Source ->
  m (Either TransformationError Source)
previewTransformation transformation src =
  (flip evalStateT) def $
    runVirtualArtistRepoT $
      runTransformPreviewT $
        transformation src

newtype TransformPreviewT m a = TransformPreviewT
  { runTransformPreviewT :: m a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadBase b, MonadBaseControl b, MonadConc, MonadCatch, MonadThrow, MonadMask, PrimMonad)
  deriving (MonadTrans, MonadTransControl) via IdentityT

type EntityTable e = Map (Repo.PrimaryKey e) e

data VirtualEntities = VirtualEntities
  { artists :: EntityTable ArtistEntity,
    artistNames :: EntityTable ArtistNameEntity,
    releases :: EntityTable ReleaseEntity,
    releaseArtists :: [ReleaseArtistNameEntity],
    sources :: EntityTable SourceEntity,
    tracks :: EntityTable TrackEntity,
    trackArtists :: [TrackArtistNameEntity]
  }
  deriving (Generic)

instance Default VirtualEntities

instance
  {-# OVERLAPS #-}
  ( FileSystem m,
    Logging m
  ) => FileSystem (TransformPreviewT m) where
  doesFileExist = lift . doesFileExist
  doesDirectoryExist = lift . doesDirectoryExist
  listDirectory = lift . listDirectory
  canonicalizePath = lift . canonicalizePath
  readFile path = lift $ do
    $(logDebugV ['path]) ("readFile called")
    FS.readFile path
  movePath source destination = lift $ do
    $(logDebugV ['source, 'destination]) "Preview movePath called"
    pure $ Right ()
  copyPath source destination = lift $ do
    $(logDebugV ['source, 'destination]) "Preview copyPath called"
    pure $ Right ()
  removePath path = lift $ do
    $(logDebugV ['path]) "Preview removePath called"
    pure $ Right ()
  removeEmptyDirectories path = lift $ do
    $(logDebugV ['path]) "Preview removeEmptyDirectories called"
    pure ()

instance
  {-# OVERLAPS #-}
  ( MetadataAggregate m,
    Logging m
  ) => MetadataAggregate (TransformPreviewT m) where
  openMetadataFile path = do
    $(logDebugV ['path]) "Preview openMetadataFile called"
    lift $ openMetadataFile path
  openMetadataFileByExt = lift . openMetadataFileByExt
  readMetadataFile mid p = lift $ readMetadataFile mid p
  writeMetadataFile mf path = lift $ do
    $(logDebugV ['path]) "Preview writeMetadataFile called"
    pure $ Right mf
  chooseMetadata ms = lift $ do
    $(logDebug) "Preview chooseMetadata called"
    chooseMetadata ms
  metadataFactory = lift . metadataFactory

instance
  {-# OVERLAPS #-}
  MultiTrack m => MultiTrack (TransformPreviewT m) where
  extractTrackTo cuefile dest =
    pure $
      Right
        F.MetadataFile
          { filePath = dest,
            metadata = mempty,
            audioInfo = cuefile.audioInfo,
            fileId = cuefile.fileId,
            pictures = []
          }

instance
  {-# OVERLAPS #-}
  MultiTrack m => MultiTrack (VirtualArtistRepoT m) where
  extractTrackTo cuefile dest =
    pure $
      Right
        F.MetadataFile
          { filePath = dest,
            metadata = mempty,
            audioInfo = cuefile.audioInfo,
            fileId = cuefile.fileId,
            pictures = []
          }

instance
  {-# OVERLAPS #-}
  ( TagMappingAggregate m,
    Logging m
  ) =>
  TagMappingAggregate (TransformPreviewT m)
  where
  resolveMappingNamed n s = TransformPreviewT do
    $(logDebug) $ "Preview resolveMappingNamed " <> showt n <> " TransformPreviewT"
    resolveMappingNamed n s
  getMappingNamed = lift . getMappingNamed
  getMappingsNamed = lift . getMappingsNamed
  getAllMappings = lift getAllMappings

newtype VirtualArtistRepoT m a = VirtualArtistRepoT
  { runVirtualArtistRepoT :: m a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadBase b, MonadBaseControl b, MonadConc, MonadCatch, MonadThrow, MonadMask, PrimMonad)
  deriving (MonadTrans, MonadTransControl) via IdentityT

deriving newtype instance (MonadState VirtualEntities m) => MonadState VirtualEntities (VirtualArtistRepoT m)

instance
  {-# OVERLAPS #-}
  ( MetadataAggregate m,
    Logging m
  ) => MetadataAggregate (VirtualArtistRepoT m) where
  openMetadataFile path = do
    $(logDebugV ['path]) "Preview openMetadataFile called"
    lift $ openMetadataFile path
  openMetadataFileByExt = lift . openMetadataFileByExt
  readMetadataFile mid p = lift $ readMetadataFile mid p
  writeMetadataFile mf path = lift $ do
    $(logDebugV ['path]) "Preview writeMetadataFile called"
    pure $ Right mf
  chooseMetadata ms = lift $ do
    $(logDebug) "Preview chooseMetadata called"
    chooseMetadata ms
  metadataFactory = lift . metadataFactory

instance
  {-# OVERLAPS #-}
  ( Repo.Repository SourceEntity m,
    Logging m
  ) =>
  Repo.Repository SourceEntity (VirtualArtistRepoT m)
  where
  getAll = lift $ Repo.getAll @SourceEntity
  getByKey = lift . Repo.getByKey @SourceEntity
  insert es = do
    $(logDebug) "Preview insert @SourceEntity called"
    pure (from <$> es)
  insert' = pure . fromIntegral . V.length
  delete _ = pure V.empty
  update e = lift $ do
    $(logDebug) "Preview update @SourceEntity called"
    pure e
  update' = void . Repo.update

instance
  {-# OVERLAPS #-}
  ( SourceRepository m,
    Logging m
  ) =>
  SourceRepository (VirtualArtistRepoT m)
  where
  getByUri = lift . Src.getByUri
  getKeysByUri = lift . Src.getKeysByUri
  getByUriPrefix = lift . Src.getByUriPrefix
  getKeysByUriPrefix = lift . Src.getKeysByUriPrefix
  getCollectionSources = lift . Src.getCollectionSources

instance
  {-# OVERLAPS #-}
  ( ArtistRepository m,
    ReleaseArtistNameRepository m,
    Logging m,
    MonadState VirtualEntities m,
    Repo.Repository ArtistNameEntity m,
    TrackRepository m,
    UuidGenerator m
  ) =>
  Repo.Repository ArtistEntity (VirtualArtistRepoT m)
  where
  getAll = VirtualArtistRepoT do
    $(logDebug) "Preview getAll @ArtistEntity called"
    artists <- uses #artists (toListOf traverse)
    all <- Repo.getAll
    pure $ V.fromList artists <> all
  getByKey keys = VirtualArtistRepoT do
    $(logDebug) "Preview getByKey @ArtistEntity called"
    artists <- gets (.artists)
    V.catMaybes <$> forM keys \key -> case artists ^. at key of
      Just v -> pure $ Just v
      Nothing -> Repo.getSingle key
  insert newArtists = do
    $(logDebug) "Preview insert @ArtistEntity called"
    forM newArtists \newArtist -> do
      artist <- case newArtist.musicBrainzId of
        Just mbid -> Artist.getByMusicBrainzId mbid
        Nothing -> V.find (\a -> isn't _Just a.musicbrainz_id) <$> getByName newArtist.name
      case artist of
        Just artist -> do
          let artist' = mergeArtist artist newArtist
          VirtualArtistRepoT ( #artists %= (at artist.id ?~ artist') )
          pure artist'
        Nothing -> do
          id <- ArtistRef <$> VirtualArtistRepoT generateV4
          let artist' = mkNewArtist id newArtist
          VirtualArtistRepoT ( #artists %= (at id ?~ artist') )
          pure artist'
  insert' newArtists = Repo.insert @ArtistEntity newArtists <&> fromIntegral . V.length
  delete _ = pure V.empty
  update e = do
    $(logDebug) "Preview update @ArtistEntity called"
    pure e
  update' _ = do
    $(logDebug) "Preview update' @ArtistEntity called"
    pure ()

instance
  {-# OVERLAPS #-}
  ( ArtistRepository m,
    ReleaseArtistNameRepository m,
    Logging m,
    MonadState VirtualEntities m,
    Repo.Repository ArtistNameEntity m,
    TrackRepository m,
    UuidGenerator m
  ) =>
  ArtistRepository (VirtualArtistRepoT m)
  where
  getByMusicBrainzId mbid = VirtualArtistRepoT do
    $(logDebug) "Preview getByMusicBrainzId @Artist called"
    artists <- uses #artists (toListOf traverse)
    case find (\a -> a.musicbrainz_id == Just (mbid.mbid)) artists of
      Just artist -> pure (Just artist)
      Nothing -> Artist.getByMusicBrainzId mbid
  getReleaseArtists releaseRef = do
    $(logDebug) "Preview getReleaseArtists called"
    artistNames <- getReleaseArtistNames releaseRef
    releaseArtists <- V.mapMaybeM (\artistName -> fmap (,artistName) <$> Repo.getSingle @ArtistEntity artistName.artist_id) artistNames
    if V.null releaseArtists
      then VirtualArtistRepoT $ getReleaseArtists releaseRef
      else pure releaseArtists
  getSourceReleaseArtists srcRef = do
    $(logDebug) "Preview getSourceReleaseArtists called"
    Track.getBySrcRef srcRef >>= \case
      Just track -> do
        $(logDebug) $ "track: " <> showt track
        getReleaseArtists track.release_id
      Nothing -> pure V.empty
  getByName name = VirtualArtistRepoT do
    artists <- uses #artists (toListOf traverse)
    case filter (\a -> a.name == name) artists of
      [] -> getByName name
      artists -> pure (V.fromList artists)

instance UuidGenerator m => UuidGenerator (VirtualArtistRepoT m) where
  generateV4 = lift generateV4

instance
  {-# OVERLAPS #-}
  ( Repo.Repository ArtistNameEntity m,
    Logging m,
    MonadState VirtualEntities m,
    UuidGenerator m
  ) =>
  Repo.Repository ArtistNameEntity (VirtualArtistRepoT m)
  where
  getAll = VirtualArtistRepoT do
    $(logDebug) "Preview getAll @ArtistNameEntity called"
    artistNames <- uses #artistNames (toListOf traverse)
    all <- Repo.getAll
    pure $ V.fromList artistNames <> all
  getByKey keys = VirtualArtistRepoT do
    $(logDebug) "Preview getByKey @ArtistNameEntity called"
    artistNames <- gets (.artistNames)
    V.catMaybes <$> forM keys \key -> case artistNames ^. at key of
      Just v -> pure $ Just v
      Nothing -> Repo.getSingle key
  insert = mapM \a -> do
    $(logDebug) "Preview insert @ArtistNameEntity called"
    id <- generateV4
    let artistName = fromNewArtistName a id
    #artistNames . at artistName.id <?= artistName
  insert' artistNames = Repo.insert @ArtistNameEntity artistNames <&> fromIntegral . V.length
  delete _ = pure V.empty
  update e = lift $ do
    $(logDebug) "Preview update @ArtistNameEntity called"
    pure e
  update' _ = lift $ do
    $(logDebug) "Preview update' @ArtistNameEntity called"
    pure ()

instance
  {-# OVERLAPS #-}
  ( ArtistNameRepository m,
    Logging m,
    MonadState VirtualEntities m,
    UuidGenerator m
  ) =>
  ArtistNameRepository (VirtualArtistRepoT m)
  where
  getArtistNames artistRef = VirtualArtistRepoT do
    $(logDebug) "Preview getArtistNames called"
    artistNames <- uses #artistNames (toListOf traverse)
    case filter (\a -> a.artist_id == artistRef) artistNames of
      [] -> getArtistNames artistRef
      an -> pure $ V.fromList an
  getAlias artistRef name = VirtualArtistRepoT do
    $(logDebug) "Preview getAlias called"
    artistNames <- uses #artistNames (toListOf traverse)
    case find (\a -> a.artist_id == artistRef && a.name == name) artistNames of
      Just artistName -> pure $ Just artistName
      Nothing -> getAlias artistRef name

instance
  {-# OVERLAPS #-}
  ( Repo.Repository ReleaseEntity m,
    Logging m,
    MonadState VirtualEntities m,
    UuidGenerator m
  ) =>
  Repo.Repository ReleaseEntity (VirtualArtistRepoT m)
  where
  getAll = VirtualArtistRepoT do
    $(logDebug) "Preview getAll @ReleaseEntity called"
    releases <- uses #releases (toListOf traverse)
    all <- Repo.getAll
    pure $ V.fromList releases <> all
  getByKey keys = VirtualArtistRepoT do
    $(logDebug) "Preview getByKey @ReleaseEntity called"
    releases <- gets (.releases)
    V.catMaybes <$> forM keys \key -> case releases ^. at key of
      Just v -> pure $ Just v
      Nothing -> Repo.getSingle key
  insert = mapM \a -> do
    $(logDebug) "Preview insert @ReleaseEntity called"
    id <- generateV4
    let release = fromNewRelease a id
    as <- #releases . at release.id <?= release
    x <- gets (.releases)
    $(logDebug) $ "Preview releases inserted: " <> showt x
    pure as
  insert' releases = Repo.insert @ReleaseEntity releases <&> fromIntegral . V.length
  delete _ = pure V.empty
  update e = lift $ do
    $(logDebug) "Preview update @ReleaseEntity called"
    pure e
  update' _ = lift $ do
    $(logDebug) "Preview update' @ReleaseEntity called"
    pure ()

instance
  {-# OVERLAPS #-}
  ( ReleaseRepository m,
    Logging m,
    MonadState VirtualEntities m,
    UuidGenerator m
  ) =>
  ReleaseRepository (VirtualArtistRepoT m)
  where
  getByMusicBrainzId mbid = VirtualArtistRepoT do
    $(logDebug) "Preview getByMusicBrainzId @Release called"
    releases <- uses #releases (toListOf traverse)
    case find (\r -> r.musicbrainz_id == Just (mbid.mbid) || r.musicbrainz_group_id == Just (mbid.mbid)) releases of
      Just release -> pure (Just release)
      Nothing -> Release.getByMusicBrainzId mbid

instance
  {-# OVERLAPS #-}
  ( ReleaseArtistNameRepository m,
    Repo.Repository ArtistNameEntity m,
    Logging m,
    MonadState VirtualEntities m
  ) =>
  ReleaseArtistNameRepository (VirtualArtistRepoT m)
  where
  getReleaseArtistNames releaseRef = VirtualArtistRepoT do
    $(logDebug) "Preview getReleaseArtistNames called"
    releaseArtists <- gets (.releaseArtists)
    artistNames <- gets (.artistNames)
    case filter (\a -> a.release_id == releaseRef) releaseArtists of
      [] -> getReleaseArtistNames releaseRef
      releaseArtists ->
        pure $
          V.fromList $
            mapMaybe (\releaseArtist -> artistNames ^. at releaseArtist.artist_name_id) releaseArtists
  insert as = do
    $(logDebug) "Preview insert @ReleaseArtistNameEntity called"
    #releaseArtists <>= V.toList as
    pure as
  insert' releaseArtists = ReleaseArtist.insert releaseArtists <&> fromIntegral . V.length

instance
  {-# OVERLAPS #-}
  ( Repo.Repository TrackEntity m,
    Logging m,
    MonadState VirtualEntities m,
    UuidGenerator m
  ) =>
  Repo.Repository TrackEntity (VirtualArtistRepoT m)
  where
  getAll = VirtualArtistRepoT do
    $(logDebug) "Preview getAll @TrackEntity called"
    tracks <- uses #tracks (toListOf traverse)
    all <- Repo.getAll
    pure $ V.fromList tracks <> all
  getByKey keys = VirtualArtistRepoT do
    $(logDebug) "Preview getByKey @TrackEntity called"
    tracks <- gets (.tracks)
    V.catMaybes <$> forM keys \key -> case tracks ^. at key of
      Just v -> pure $ Just v
      Nothing -> Repo.getSingle key
  insert = mapM \t -> do
    $(logDebug) "Preview insert @TrackEntity called"
    id <- generateV4
    let track = fromNewTrack t id
    #tracks . at track.id <?= track
  insert' tracks = Repo.insert @TrackEntity tracks <&> fromIntegral . V.length
  delete _ = pure V.empty
  update = mapM \track -> do
    $(logDebug) "Preview update @TrackEntity called"
    #tracks . at track.id <?= track
  update' = mapM_ \track -> do
    $(logDebug) "Preview update' @TrackEntity called"
    #tracks . at track.id ?= track

instance
  {-# OVERLAPS #-}
  ( TrackRepository m,
    Logging m,
    MonadState VirtualEntities m,
    UuidGenerator m
  ) =>
  TrackRepository (VirtualArtistRepoT m)
  where
  getByMusicBrainzId mbid = VirtualArtistRepoT do
    $(logDebug) "Preview getByMusicBrainzId @Track called"
    tracks <- uses #tracks (toListOf traverse)
    case find (\t -> t.musicbrainz_id == Just mbid.mbid) tracks of
      Just track -> pure $ Just track
      Nothing -> Track.getByMusicBrainzId mbid
  getBySrcRef srcRef = VirtualArtistRepoT do
    $(logDebug) "Preview getBySrcRef @Track called"
    tracks <- uses #tracks (toListOf traverse)
    case find (\t -> t.source_id == srcRef) tracks of
      Just track -> pure $ Just track
      Nothing -> Track.getBySrcRef srcRef

instance
  {-# OVERLAPS #-}
  ( TrackArtistNameRepository m,
    Logging m,
    MonadState VirtualEntities m
  ) =>
  TrackArtistNameRepository (VirtualArtistRepoT m)
  where
  getTrackArtistNames trackRef = VirtualArtistRepoT do
    $(logDebug) "Preview getTrackArtistNames called"
    trackArtists <- gets (.trackArtists)
    artistNames <- gets (.artistNames)
    case filter (\a -> a.track_id == trackRef) trackArtists of
      [] -> getTrackArtistNames trackRef
      trackArtists ->
        pure $
          V.fromList $
            mapMaybe (\trackArtist -> artistNames ^. at trackArtist.artist_name_id) trackArtists
  insert ts = do
    $(logDebug) "Preview insert @TrackArtistNameEntity called"
    #trackArtists <>= V.toList ts
    pure ts
  insert' trackArtists = TrackArtist.insert trackArtists <&> fromIntegral . V.length

instance
  {-# OVERLAPS #-}
  ( TagMappingAggregate m,
    Logging m
  ) =>
  TagMappingAggregate (VirtualArtistRepoT m)
  where
  resolveMappingNamed n s = VirtualArtistRepoT do
    $(logDebug) $ "Preview resolveMappingNamed " <> showt n <> " VirtualArtistRepoT"
    resolveMappingNamed n s
  getMappingNamed = lift . getMappingNamed
  getMappingsNamed = lift . getMappingsNamed
  getAllMappings = lift getAllMappings

instance Repo.Repository TagMappingEntity m => Repo.Repository TagMappingEntity (VirtualArtistRepoT m) where
  getAll = lift Repo.getAll
  getByKey = lift . Repo.getByKey
  insert = lift . Repo.insert
  insert' = lift . Repo.insert' @TagMappingEntity
  delete = lift . Repo.delete @TagMappingEntity
  update = lift . Repo.update
  update' = lift . Repo.update'

instance
  {-# OVERLAPS #-}
  ( ReleaseAggregate m,
    Logging m
  ) =>
  ReleaseAggregate (TransformPreviewT m)
  where
  importReleases srcs = TransformPreviewT do
    $(logDebug) "Preview importReleases called"
    importReleases srcs
  getRelease ref = TransformPreviewT do
    $(logDebug) "Preview getRelease called"
    Release.getRelease ref

instance
  {-# OVERLAPS #-}
  ( ReleaseAggregate m,
    Logging m
  ) =>
  ReleaseAggregate (VirtualArtistRepoT m)
  where
  importReleases srcs = VirtualArtistRepoT do
    $(logDebug) "Preview importReleases called"
    importReleases srcs
  getRelease ref = VirtualArtistRepoT do
    $(logDebug) "Preview getRelease called"
    Release.getRelease ref

instance
  {-# OVERLAPS #-}
  ( Covers.CoverService m,
    Logging m
  ) =>
  Covers.CoverService (TransformPreviewT m)
  where
  searchForCovers ref = TransformPreviewT do
    $(logDebug) "Preview searchForCovers TransformPreviewT"
    Covers.searchForCovers ref
  copyCoverToDir src dest = TransformPreviewT do
    $(logDebug) "Preview copyCoverToDir TransformPreviewT"
    Covers.copyCoverToDir src dest

instance
  {-# OVERLAPS #-}
  ( Covers.CoverService m,
    Logging m
  ) =>
  Covers.CoverService (VirtualArtistRepoT m)
  where
  searchForCovers ref = VirtualArtistRepoT do
    $(logDebug) "Preview searchForCovers VirtualArtistRepoT"
    Covers.searchForCovers ref
  copyCoverToDir src dest = VirtualArtistRepoT do
    $(logDebug) "Preview copyCoverToDir VirtualArtistRepoT"
    Covers.copyCoverToDir src dest

instance
  {-# OVERLAPS #-}
  Logging m => FileSystemWatcher (TransformPreviewT m) where
  startWatching _ _ = $(logDebug) "Preview startWarching TransformPreviewT"
  stopWatching _ = $(logDebug) "Preview stopWatching TransformPreviewT"

instance
  {-# OVERLAPS #-}
  Logging m => FileSystemWatchLocks (TransformPreviewT m) where
  lockPathsDuring _ m = do
    $(logDebug) "Preview lockPathsDuring TransformPreviewT"
    m

instance
  {-# OVERLAPS #-}
  Logging m => FileSystemWatcher (VirtualArtistRepoT m) where
  startWatching _ _ = $(logDebug) "Preview startWarching VirtualArtistRepoT"
  stopWatching _ = $(logDebug) "Preview stopWatching VirtualArtistRepoT"

instance
  {-# OVERLAPS #-}
  Logging m => FileSystemWatchLocks (VirtualArtistRepoT m) where
  lockPathsDuring _ m = do
    $(logDebug) "Preview lockPathsDuring VirtualArtistRepoT"
    m

instance
  {-# OVERLAPS #-}
  AppDataReader m => AppDataReader (TransformPreviewT m) where
  alterAppData' = lift . alterAppData
  getAppData' = lift . getAppData'
  putAppData = lift . putAppData
  deleteAppData' = lift . deleteAppData'

instance
  {-# OVERLAPS #-}
  AppDataReader m => AppDataReader (VirtualArtistRepoT m) where
  alterAppData' = lift . alterAppData
  getAppData' = lift . getAppData'
  putAppData = lift . putAppData
  deleteAppData' = lift . deleteAppData'

instance
  {-# OVERLAPS #-}
  Repo.Repository CollectionEntity m => Repo.Repository CollectionEntity (TransformPreviewT m) where
  getAll = lift Repo.getAll
  getByKey = lift . Repo.getByKey
  insert = lift . Repo.insert
  insert' = lift . Repo.insert' @CollectionEntity
  delete = lift . Repo.delete @CollectionEntity
  update = lift . Repo.update
  update' = lift . Repo.update'

instance
  {-# OVERLAPS #-}
  Repo.Repository CollectionEntity m => Repo.Repository CollectionEntity (VirtualArtistRepoT m) where
  getAll = lift Repo.getAll
  getByKey = lift . Repo.getByKey
  insert = lift . Repo.insert
  insert' = lift . Repo.insert' @CollectionEntity
  delete = lift . Repo.delete @CollectionEntity
  update = lift . Repo.update
  update' = lift . Repo.update'

instance
  {-# OVERLAPS #-}
  CollectionRepository m => CollectionRepository (TransformPreviewT m) where
  getByUri = lift . Collection.getByUri

instance
  {-# OVERLAPS #-}
  CollectionRepository m => CollectionRepository (VirtualArtistRepoT m) where
  getByUri = lift . Collection.getByUri

instance (SourceAggregate m, Logging m) => SourceAggregate (VirtualArtistRepoT m) where
  importSources = lift . importSources
  updateSource s = do
    $(logDebug) "Preview updateSource VirtualArtistRepoT"
    pure $ Right s

data TransformationError
  = MoveTransformError SourceFileManipError
  | MetadataTransformError F.MetadataException
  | UnknownTagMapping Text
  | UnsupportedSourceKind
  | CollectionNotFound CollectionRef
  | SourceNotFound SourceRef
  | ImportFailed SourceError
  | MultiTrackError MultiTrackError
  | SourceConversionError (TryFromException SourceEntity Source)
  | UnsupportedTransform Text
  | MetadataConversionError F.MetadataId F.MetadataId
  | UnknownLength
  deriving (Show)
  deriving TextShow via FromStringShow TransformationError

instance Exception TransformationError

instance From SourceFileManipError TransformationError where
  from = MoveTransformError

instance From F.MetadataException TransformationError where
  from = MetadataTransformError

instance From (TryFromException SourceEntity Source) TransformationError where
  from = SourceConversionError

moveSourceWithPattern ::
  ( FileSystem m,
    CollectionRepository m,
    SourceRepository m,
    TagMappingAggregate m,
    FileSystemWatchLocks m,
--    Tracing m,
    Logging m
  ) =>
  Maybe CollectionRef ->
  NonEmpty SourcePathPattern ->
  Source ->
  m (Either SourceFileManipError Source)
moveSourceWithPattern collectionRef pats src@Source {ref, source} =
  case uriToFilePath source of
    Just srcPath ->
      previewSourceMoveWithPattern (fromMaybe src.collectionRef collectionRef) pats src >>= \case
        Just destPath | destPath == srcPath -> pure $ Right src
        Just destPath -> lockPathsDuring (srcPath :| [destPath]) do
          let SourceRef id = ref
          $(logInfo) $ "moving source " <> showt id <> " from " <> showt srcPath <> " to " <> showt destPath
          movePath srcPath destPath >>= \case
            Left e -> pure $ Left (from e)
            Right _ -> do
              $(logInfo) $ "successfully moved source " <> showt id <> " from " <> showt srcPath <> " to " <> showt destPath
              findCoverImage (takeDirectory srcPath) >>= \case
                Nothing -> pure ()
                Just imagePath -> do
                  let destImagePath = replaceDirectory imagePath (takeDirectory destPath)
                  movePath imagePath destImagePath >>= \case
                    Left e -> do
                      let cause = displayException e
                      $(logErrorV ['cause]) $
                        "failed to move cover image "
                          <> showt (takeFileName imagePath)
                          <> " for source "
                          <> showt id
                    Right _ ->
                      $(logInfo) $ "successfully moved cover image " <> showt (takeFileName imagePath) <> " for source " <> showt id
              void $ removeEmptyDirectories (takeDirectory srcPath)
              let movedSrc = src & #source .~ fileUri destPath
              Repo.updateSingle @SourceEntity (from movedSrc) >>= \case
                Just updatedSrc -> pure $ first ConversionError $ tryFrom updatedSrc
                Nothing -> pure $ Left SourceUpdateError
        Nothing -> pure $ Left PatternError
    Nothing -> pure $ Left SourcePathError

previewSourceMoveWithPattern ::
  ( CollectionRepository m,
    TagMappingAggregate m,
    Logging m
  ) =>
  CollectionRef ->
  NonEmpty SourcePathPattern ->
  Source ->
  m (Maybe FilePath)
previewSourceMoveWithPattern collectionRef pats src@Source {source} =
  Repo.getSingle @CollectionEntity collectionRef >>= \case
    Just CollectionTable {root_uri} -> case parseURI (T.unpack root_uri) >>= uriToFilePath of
      Just rootPath -> do
        srcPath <- renderSourcePath rootPath src pats
        pure $ Just $ srcPath <> takeExtension (show source)
      Nothing -> pure Nothing
    Nothing -> pure Nothing

renderSourcePath ::
  ( TagMappingAggregate m,
    Logging m
  ) =>
  FilePath ->
  Source ->
  NonEmpty SourcePathPattern ->
  m FilePath
renderSourcePath basepath src pats =
  let (<</>>) = liftM2 (</>)
   in pure basepath
        <</>> fromMaybe ""
        <$> Fold.foldM (Fold.sink (renderSourcePattern src)) pats

renderSourcePattern ::
  ( TagMappingAggregate m,
    Logging m
  ) =>
  Source ->
  SourcePathPattern ->
  m (Maybe FilePath)
renderSourcePattern src = \case
  LiteralPattern p -> pure $ Just (T.unpack p)
  GroupPattern pats -> do
    ts <- forM pats (renderSourcePattern src)
    let x = foldl' appendJust (Just "") ts
    pure $ Just (fromMaybe "" x)
  MappingPattern mappingName -> do
    rs <- resolveMappingNamed mappingName src
    pure $ T.unpack <$> formatList (T.replace "/" "," <$> V.toList rs)
  DefaultPattern a b -> renderSourcePattern src a <<|>> renderSourcePattern src b
  PrintfPattern fmt pat ->
    fmap (printf fmt) <$> renderSourcePattern src pat
  where
    formatList :: [Text] -> Maybe Text
    formatList [] = Nothing
    formatList [a] = Just a
    formatList (a : [b]) = Just (a <> " & " <> b)
    formatList (a : as) = (\b -> a <> ", " <> b) <$> formatList as
    appendJust (Just a) (Just b) = Just (a <> b)
    appendJust _ _ = Nothing

parseMovePattern :: Text -> Either (Maybe Text) (NonEmpty SourcePathPattern)
parseMovePattern s = parse >>= nonEmptyRight
  where
    parse = case P.parse terms s of
      P.Fail _ _ctx e -> Left (Just (T.pack e))
      P.Partial _ -> Left Nothing
      P.Done _ r -> Right r
    nonEmptyRight :: [SourcePathPattern] -> Either (Maybe Text) (NonEmpty SourcePathPattern)
    nonEmptyRight (p : ps) = Right $ p :| ps
    nonEmptyRight [] = Left Nothing
    terms = many term
    term = P.try format <|> P.try group <|> P.try literal
    format = do
      void (P.char '%')
      padZero <- isn't _Nothing <$> P.option Nothing (Just <$> P.char '0')
      width <- fmap (: []) <$> P.option Nothing (Just <$> P.digit)
      mapping' <- mapping
      case width of
        Just width' ->
          pure $
            PrintfPattern ("%" <> (if padZero then "0" else ".") <> width' <> "s") $
              MappingPattern mapping'
        Nothing -> pure $ MappingPattern mapping'
    mapping = P.takeWhile1 (\c -> c == '_' || isLetter c) <?> "mapping"
    group = do
      void (P.char '[')
      P.manyTill term (P.char ']') >>= \case
        (t : ts) -> pure $ GroupPattern (t :| ts)
        [] -> fail "no terms in group"
    literal =
      LiteralPattern
        <$> (P.takeWhile1
                 (\c -> isAlphaNum c || isSpace c || not (isReservedSymbol c) && isPunctuation c)
                <?> "literal"
            )
    isReservedSymbol c = elem c ['[', ']', '%']

extractTrack ::
  MonadSourceTransform m =>
  Maybe CollectionRef ->
  NonEmpty SourcePathPattern ->
  Source ->
  m (Either TransformationError Source)
extractTrack collectionRef' patterns s@Source {multiTrack = Just MultiTrackDesc {..}, metadata = Just metadata} =
  let collectionRef = fromMaybe s.collectionRef collectionRef'
   in case uriToFilePath s.source of
    Just filePath ->
      Repo.getSingle @CollectionEntity collectionRef >>= \case
        Nothing -> pure $ Left (CollectionNotFound collectionRef)
        Just CollectionTable {root_uri} -> case uriToFilePath =<< parseURI (T.unpack root_uri) of
          Nothing -> pure $ Left UnsupportedSourceKind
          Just basePath -> do
            mappings <- V.fromList . toListOf traverse <$> getAllMappings
            srcPath <- renderSourcePath basePath s patterns
            let dest = addExtension srcPath (takeExtension filePath)
            lockPathsDuring (dest :| []) $ try do
              mf <- openMetadataFile filePath >>= throwOnLeft . first MetadataTransformError
              let cuefile =
                    CueFileSource
                      { idx,
                        range,
                        filePath,
                        metadata,
                        audioInfo = mf.audioInfo,
                        fileId = s.kind,
                        cueFilePath = filePath,
                        pictures = mf.pictures
                      }
              raw <- extractTrackTo cuefile dest >>= throwOnLeft . first MultiTrackError
              $(logDebug) $ "Mappings: " <> showt mappings
              let vc =
                    fromMaybe (F.metadataFactory @F.VorbisComments F.emptyTags) $
                      F.convert' F.vorbisCommentsId metadata mappings
              $(logDebug) $ "Original metadata: " <> showt metadata
              $(logDebug) $ "Converted metadata: " <> showt vc
              let m = HashMap.fromList [(F.vorbisCommentsId, vc)]
              let metadataFile = raw & #metadata .~ m
              mf <- writeMetadataFile metadataFile dest >>= throwOnLeft . first MetadataTransformError
              srcs <- importSources (V.singleton (FileSource collectionRef mf))
              case srcs ^? _head of
                Just src -> pure src
                Nothing -> throwM (ImportFailed (ImportSourceError Nothing))
    Nothing -> pure $ Right s
extractTrack _ _ src = pure $ Right src

data MetadataTransformation
  = SetMapping { mappingName :: Text, values :: Vector Text }
  | RemoveMappings (Vector Text)
  | RetainMappings (Vector Text)
  | AddTag {mappingName :: Text, value :: Text}
  | RemoveTag {mappingName :: Text, value :: Text}
  | RemoveTags {mappingName :: Text}
  | RemoveAll
  deriving Generic
  deriving TextShow via FromGeneric MetadataTransformation
  deriving (FromJSON, ToJSON) via CustomJSON JSONOptions MetadataTransformation

editMetadata :: MonadSourceTransform m => Vector MetadataTransformation -> Source -> m (Either TransformationError Source)
editMetadata _ src@Source {metadata = Nothing} = pure $ Right src
editMetadata ts src = {-withSpan "editMetadata" defaultSpanArguments $-} try do
  src' <- foldM (\src t -> throwOnLeft =<< editMetadata' t src) src ts
  if src' /= src then
    updateSource src' >>= throwOnLeft . first ImportFailed
  else pure src'
  where
  editMetadata' _ src@Source {metadata = Nothing} = pure $ Right src
  editMetadata' (SetMapping mappingName vs) src@Source {metadata = Just metadata'} =
    try do
      mapping <- getMappingNamed mappingName >>= throwOnNothing (UnknownTagMapping mappingName)
      let metadata = into @SourceMetadata metadata'.tags
          current  = metadata' ^. F.tagLens mapping in
        $(logDebugV ['mapping, 'metadata, 'current]) $ "Setting tag to " <> showt vs
      let newMetadata = metadata' & F.tagLens mapping .~ vs
      pure (src & #metadata ?~ newMetadata)
  editMetadata' (RemoveMappings mappings) src = flatten <$> (forM mappings $ \mapping -> editMetadata' (SetMapping mapping V.empty) src)
    where
      flatten es = foldl' (\_a b -> b) (Left $ ImportFailed undefined) es
  editMetadata' (RetainMappings retained) src@Source {metadata = Just metadata} =
    let tag = F.mappedTag metadata.mappingSelector
     in try do
          retainedMappings <- forM retained $ \mappingName -> do
            getMappingNamed mappingName
              >>= throwOnNothing (UnknownTagMapping mappingName)
          let newTags = foldl' (\ts mapping -> ts & tag mapping .~ (metadata.tag mapping)) F.emptyTags retainedMappings
          let newMetadata = metadata {F.tags = newTags}
          pure (src & #metadata ?~ newMetadata)
  editMetadata' (AddTag k v) src@Source {metadata = Just metadata} = try do
    let (F.Tags tags) = metadata.tags
    let newMetadata = metadata {F.tags = F.Tags (V.snoc tags (k, v))}
    pure (src & #metadata ?~ newMetadata)
  editMetadata' (RemoveTag k v) src@Source {metadata = Just metadata} = try do
    let (F.Tags tags) = metadata.tags
    let newMetadata = metadata {F.tags = F.Tags (V.filter (\(k', v') -> k' /= k && v' /= v) tags)}
    pure (src & #metadata ?~ newMetadata)
  editMetadata' (RemoveTags k) src@Source {metadata = Just metadata} = try do
    let (F.Tags tags) = metadata.tags
    let newMetadata = metadata {F.tags = F.Tags (V.filter (\(k', _) -> k' /= k) tags)}
    pure (src & #metadata ?~ newMetadata)
  editMetadata' RemoveAll src@Source {metadata = Just metadata} = try do
    let newMetadata = metadata {F.tags = F.Tags V.empty}
    pure (src & #metadata ?~ newMetadata)

musicBrainzLookup ::
  ( MB.MusicBrainzService m,
    Logging m,
    SourceAggregate m
  ) =>
  Source -> m (Either TransformationError Source)
musicBrainzLookup src@Source {metadata = Nothing} = do
  $(logDebug) $ "No Metadata; Cannot lookup MusicBrainz by metadata for source " <> showt src.ref
  pure $ Right src
musicBrainzLookup src@Source {metadata = Just metadata} = (flip evalStateT) metadata do
  get >>= MB.getReleaseAndGroup >>= \case
    (Nothing, Nothing) -> do
      $(logInfo) $ "No release or release-group found for source " <> showt src.ref
      pure ()
    (releaseGroup, Just release) -> do
      $(logInfo) $ "release found for source " <> showt src.ref
      F.tagLens MB.releaseIdTag .= V.singleton release.id.mbid
      case releaseGroup ^? _Just . #id . coerced of
        Just releaseGroupId -> F.tagLens MB.releaseGroupIdTag .= V.singleton releaseGroupId
        _ -> pure ()
      let releaseArtists = release.artistCredit ^.. _Just . traverse . (to (.artist.id.mbid))
      F.tagLens MB.albumArtistIdTag .= V.fromList releaseArtists
      case release.labelInfo ^? _Just . traverse . to (.catalogNumber) . _Just of
        Just catNum -> F.tagLens M.catalogNumber .= V.singleton catNum
        _ -> pure ()
    (Just releaseGroup, Nothing) -> do
      $(logInfo) $ "release-group found for source " <> showt src.ref
      F.tagLens MB.releaseGroupIdTag .= V.singleton releaseGroup.id.mbid
  get >>= MB.getRecordingFromMetadata >>= \case
    Just recording -> do
      F.tagLens MB.recordingIdTag .= V.singleton recording.id.mbid
      let trackArtists = recording.artistCredit ^.. _Just . traverse . (to (.artist.id.mbid))
      F.tagLens MB.artistIdTag .= V.fromList trackArtists
    _ -> pure ()
  newMetadata <- get
  $(logDebug) $ "Updating source with musicbrainz metadata " <> showt newMetadata
  first ImportFailed <$> updateSource (src & #metadata .~ Just newMetadata)

copyCoverImage :: MonadSourceTransform m => URI -> Source -> m (Either TransformationError Source)
copyCoverImage imageUrl src =
  case uriToFilePath src.source of
    Just srcPath -> do
      Covers.copyCoverToDir imageUrl (takeDirectory srcPath)
      pure (Right src)
    Nothing -> pure (Right src)

convertMetadataFormat :: MonadSourceTransform m => F.MetadataId -> Source -> m (Either TransformationError Source)
convertMetadataFormat mid src = do
  !mappings <- V.fromList . toListOf traverse <$> getAllMappings

  case flip (F.convert' mid) mappings =<< src.metadata of
    Just metadata -> pure $ Right (src & #metadata ?~ metadata)
    Nothing -> pure $ Left $ MetadataConversionError (fromMaybe nullMetadata $ src.metadata <&> (.formatId)) mid
