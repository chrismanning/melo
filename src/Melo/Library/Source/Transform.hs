{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Source.Transform where

import Control.Applicative hiding (many, some)
import Control.Concurrent.Classy
import Melo.Common.Exception as E hiding (try)
import Control.Foldl qualified as Fold
import Control.Lens (to)
import Control.Monad.Except
import Control.Monad.State.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict (StateT, evalStateT)
import Data.Char
import Data.Default
import Data.Either.Combinators
import Data.HashMap.Strict qualified as H
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Void (Void)
import GHC.Generics hiding (from, to)
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
import Melo.Library.Collection.Repo
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
import System.IO (TextEncoding)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Printf

type Transform m = Source -> m (Either TransformationError Source)

data TransformAction where
  Move :: Maybe CollectionRef -> NonEmpty SourcePathPattern -> TransformAction
  Copy :: Maybe CollectionRef -> NonEmpty SourcePathPattern -> TransformAction
  ExtractEmbeddedImage :: URI -> TransformAction
  EmbedImage :: URI -> TransformAction
  CopyCoverImage :: URI -> TransformAction
  SplitMultiTrackFile :: Maybe CollectionRef -> NonEmpty SourcePathPattern -> TransformAction
  RemoveOtherFiles :: TransformAction
  MusicBrainzLookup :: TransformAction
  ConvertEncoding :: TextEncoding -> TransformAction
  EditMetadata :: Vector MetadataTransformation -> TransformAction
  ConvertMetadataFormat :: F.MetadataId -> TransformAction
  ConvertFileFormat :: F.MetadataFileId -> TransformAction

deriving instance Show TransformAction
deriving via (FromStringShow TransformAction) instance TextShow TransformAction

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
evalTransformAction (Move ref patterns) src = mapLeft from <$> moveSourceWithPattern ref patterns src
evalTransformAction (SplitMultiTrackFile ref patterns) src = mapLeft from <$> extractTrack ref patterns src
evalTransformAction (EditMetadata metadataTransformations) src = mapLeft from <$> editMetadata metadataTransformations src
evalTransformAction MusicBrainzLookup src = musicBrainzLookup src
evalTransformAction (CopyCoverImage coverUrl) src = copyCoverImage coverUrl src
evalTransformAction (ConvertMetadataFormat mid) src = convertMetadataFormat mid src
evalTransformAction t _ = pure $ Left (UnsupportedTransform (showt t))

type MonadSourceTransform m =
  ( CollectionRepository m,
    FileSystem m,
    FileSystemWatcher m,
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
        ( SourceAggregateIOT
            ( ReleaseAggregateIOT
                ( TrackAggregateIOT
                    ( ArtistAggregateIOT
                        ( TagMappingAggregateT
                            (VirtualArtistRepoT (StateT VirtualEntities m))
                        )
                    )
                )
            )
        )
    ) ->
  Source ->
  m (Either TransformationError Source)
previewTransformation transformation src =
  (flip evalStateT) def $
    runVirtualArtistRepoT $
      runTagMappingAggregate $
        runArtistAggregateIOT $
          runTrackAggregateIOT $
            runReleaseAggregateIOT $
              runSourceAggregateIOT $
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

instance MonadSourceTransform m => FileSystem (TransformPreviewT m) where
  doesFileExist = lift . doesFileExist
  doesDirectoryExist = lift . doesDirectoryExist
  listDirectory = lift . listDirectory
  canonicalizePath = lift . canonicalizePath
  readFile p = lift $ do
    $(logDebug) ("readFile called with " <> showt p)
    FS.readFile p
  movePath _ _ = lift $ do
    $(logDebug) "Preview movePath called"
    pure $ Right ()
  copyPath _ _ = lift $ do
    $(logDebug) "Preview copyPath called"
    pure $ Right ()
  removePath _ = lift $ do
    $(logDebug) "Preview removePath called"
    pure $ Right ()
  removeEmptyDirectories _ = lift $ do
    $(logDebug) "Preview removeEmptyDirectories called"
    pure ()

instance MonadSourceTransform m => MetadataAggregate (TransformPreviewT m) where
  openMetadataFile _p = do
    $(logDebug) "Preview openMetadataFile called"
    lift $ openMetadataFile _p
  openMetadataFileByExt = lift . openMetadataFileByExt
  readMetadataFile mid p = lift $ readMetadataFile mid p
  writeMetadataFile mf _p = lift $ do
    $(logDebug) "Preview writeMetadataFile called"
    pure $ Right mf
  chooseMetadata ms = lift $ do
    $(logDebug) "Preview chooseMetadata called"
    chooseMetadata ms
  metadataFactory = lift . metadataFactory

instance MonadSourceTransform m => MultiTrack (TransformPreviewT m) where
  extractTrackTo cuefile dest =
    pure $
      Right
        F.MetadataFile
          { filePath = dest,
            metadata = H.empty,
            audioInfo = cuefile.audioInfo,
            fileId = cuefile.fileId,
            pictures = []
          }

instance
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

deriving instance (MonadState VirtualEntities m) => MonadState VirtualEntities (VirtualArtistRepoT m)

instance MonadSourceTransform m => MetadataAggregate (VirtualArtistRepoT m) where
  openMetadataFile _p = do
    $(logDebug) "Preview openMetadataFile called"
    lift $ openMetadataFile _p
  openMetadataFileByExt = lift . openMetadataFileByExt
  readMetadataFile mid p = lift $ readMetadataFile mid p
  writeMetadataFile mf _p = lift $ do
    $(logDebug) "Preview writeMetadataFile called"
    pure $ Right mf
  chooseMetadata ms = lift $ do
    $(logDebug) "Preview chooseMetadata called"
    chooseMetadata ms
  metadataFactory = lift . metadataFactory

instance
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
  insert' = pure . V.length
  delete _ = pure V.empty
  update e = lift $ do
    $(logDebug) "Preview update @SourceEntity called"
    pure e
  update' = void . Repo.update

instance
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
    artists <- uses #artists Map.elems
    all <- Repo.getAll
    pure $ V.fromList artists <> all
  getByKey keys = VirtualArtistRepoT do
    $(logDebug) "Preview getByKey @ArtistEntity called"
    artists <- gets (.artists)
    V.catMaybes <$> forM keys \key -> case Map.lookup key artists of
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
          VirtualArtistRepoT ( #artists %= Map.insert artist.id artist' )
          pure artist'
        Nothing -> do
          id <- ArtistRef <$> VirtualArtistRepoT generateV4
          let artist' = mkNewArtist id newArtist
          VirtualArtistRepoT ( #artists %= Map.insert id artist' )
          pure artist'
  insert' newArtists = Repo.insert @ArtistEntity newArtists <&> V.length
  delete _ = pure V.empty
  update e = do
    $(logDebug) "Preview update @ArtistEntity called"
    pure e
  update' _ = do
    $(logDebug) "Preview update' @ArtistEntity called"
    pure ()

instance
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
    artists <- uses #artists Map.elems
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
  getSourceReleaseArtists srcRef = fmap (fromMaybe V.empty) $ runMaybeT do
    $(logDebug) "Preview getSourceReleaseArtists called"
    track <- MaybeT $ Track.getBySrcRef srcRef
    $(logDebug) $ "track: " <> showt track
    getReleaseArtists track.release_id
  getByName name = VirtualArtistRepoT do
    artists <- uses #artists Map.elems
    case filter (\a -> a.name == name) artists of
      [] -> getByName name
      artists -> pure (V.fromList artists)

instance
  ( Repo.Repository ArtistNameEntity m,
    Logging m,
    MonadState VirtualEntities m,
    UuidGenerator m
  ) =>
  Repo.Repository ArtistNameEntity (VirtualArtistRepoT m)
  where
  getAll = VirtualArtistRepoT do
    $(logDebug) "Preview getAll @ArtistNameEntity called"
    artistNames <- uses #artistNames Map.elems
    all <- Repo.getAll
    pure $ V.fromList artistNames <> all
  getByKey keys = VirtualArtistRepoT do
    $(logDebug) "Preview getByKey @ArtistNameEntity called"
    artistNames <- gets (.artistNames)
    V.catMaybes <$> forM keys \key -> case Map.lookup key artistNames of
      Just v -> pure $ Just v
      Nothing -> Repo.getSingle key
  insert = mapM \a -> do
    $(logDebug) "Preview insert @ArtistNameEntity called"
    id <- generateV4
    let artistName = fromNewArtistName a id
    #artistNames . at artistName.id <?= artistName
  insert' artistNames = Repo.insert @ArtistNameEntity artistNames <&> V.length
  delete _ = pure V.empty
  update e = lift $ do
    $(logDebug) "Preview update @ArtistNameEntity called"
    pure e
  update' _ = lift $ do
    $(logDebug) "Preview update' @ArtistNameEntity called"
    pure ()

instance
  ( ArtistNameRepository m,
    Logging m,
    MonadState VirtualEntities m,
    UuidGenerator m
  ) =>
  ArtistNameRepository (VirtualArtistRepoT m)
  where
  getArtistNames artistRef = VirtualArtistRepoT do
    artistNames <- uses #artistNames Map.elems
    case filter (\a -> a.artist_id == artistRef) artistNames of
      [] -> getArtistNames artistRef
      an -> pure $ V.fromList an
  getAlias artistRef name = VirtualArtistRepoT do
    artistNames <- uses #artistNames Map.elems
    case find (\a -> a.artist_id == artistRef && a.name == name) artistNames of
      Just artistName -> pure $ Just artistName
      Nothing -> getAlias artistRef name

instance
  ( Repo.Repository ReleaseEntity m,
    Logging m,
    MonadState VirtualEntities m,
    UuidGenerator m
  ) =>
  Repo.Repository ReleaseEntity (VirtualArtistRepoT m)
  where
  getAll = VirtualArtistRepoT do
    $(logDebug) "Preview getAll @ReleaseEntity called"
    releases <- uses #releases Map.elems
    all <- Repo.getAll
    pure $ V.fromList releases <> all
  getByKey keys = VirtualArtistRepoT do
    $(logDebug) "Preview getByKey @ReleaseEntity called"
    releases <- gets (.releases)
    V.catMaybes <$> forM keys \key -> case Map.lookup key releases of
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
  insert' releases = Repo.insert @ReleaseEntity releases <&> V.length
  delete _ = pure V.empty
  update e = lift $ do
    $(logDebug) "Preview update @ReleaseEntity called"
    pure e
  update' _ = lift $ do
    $(logDebug) "Preview update' @ReleaseEntity called"
    pure ()

instance
  ( ReleaseRepository m,
    Logging m,
    MonadState VirtualEntities m,
    UuidGenerator m
  ) =>
  ReleaseRepository (VirtualArtistRepoT m)
  where
  getByMusicBrainzId mbid = VirtualArtistRepoT do
    releases <- uses #releases Map.elems
    case find (\r -> r.musicbrainz_id == Just (mbid.mbid) || r.musicbrainz_group_id == Just (mbid.mbid)) releases of
      Just release -> pure (Just release)
      Nothing -> Release.getByMusicBrainzId mbid

instance
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
            mapMaybe (\releaseArtist -> Map.lookup releaseArtist.artist_name_id artistNames) releaseArtists
  insert as = do
    $(logDebug) "Preview insert @ReleaseArtistNameEntity called"
    #releaseArtists <>= V.toList as
    pure as
  insert' releaseArtists = ReleaseArtist.insert releaseArtists <&> V.length

instance
  ( Repo.Repository TrackEntity m,
    Logging m,
    MonadState VirtualEntities m,
    UuidGenerator m
  ) =>
  Repo.Repository TrackEntity (VirtualArtistRepoT m)
  where
  getAll = VirtualArtistRepoT do
    $(logDebug) "Preview getAll @TrackEntity called"
    tracks <- uses #tracks Map.elems
    all <- Repo.getAll
    pure $ V.fromList tracks <> all
  getByKey keys = VirtualArtistRepoT do
    $(logDebug) "Preview getByKey @TrackEntity called"
    tracks <- gets (.tracks)
    V.catMaybes <$> forM keys \key -> case Map.lookup key tracks of
      Just v -> pure $ Just v
      Nothing -> Repo.getSingle key
  insert = mapM \t -> do
    $(logDebug) "Preview insert @TrackEntity called"
    id <- generateV4
    let track = fromNewTrack t id
    #tracks . at track.id <?= track
  insert' tracks = Repo.insert @TrackEntity tracks <&> V.length
  delete _ = pure V.empty
  update = mapM \track -> do
    $(logDebug) "Preview update @TrackEntity called"
    #tracks . at track.id <?= track
  update' = mapM_ \track -> do
    $(logDebug) "Preview update' @TrackEntity called"
    #tracks . at track.id ?= track

instance
  ( TrackRepository m,
    Logging m,
    MonadState VirtualEntities m,
    UuidGenerator m
  ) =>
  TrackRepository (VirtualArtistRepoT m)
  where
  getByMusicBrainzId mbid = VirtualArtistRepoT do
    tracks <- uses #tracks Map.elems
    case find (\t -> t.musicbrainz_id == Just mbid.mbid) tracks of
      Just track -> pure $ Just track
      Nothing -> Track.getByMusicBrainzId mbid
  getBySrcRef srcRef = VirtualArtistRepoT do
    tracks <- uses #tracks Map.elems
    case find (\t -> t.source_id == srcRef) tracks of
      Just track -> pure $ Just track
      Nothing -> Track.getBySrcRef srcRef

instance
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
            mapMaybe (\trackArtist -> Map.lookup trackArtist.artist_name_id artistNames) trackArtists
  insert ts = do
    $(logDebug) "Preview insert @TrackArtistNameEntity called"
    #trackArtists <>= V.toList ts
    pure ts
  insert' trackArtists = TrackArtist.insert trackArtists <&> V.length

instance
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

instance TagMappingRepository m => TagMappingRepository (VirtualArtistRepoT m)

instance Repo.Repository TagMappingEntity m => Repo.Repository TagMappingEntity (VirtualArtistRepoT m) where
  getAll = lift Repo.getAll
  getByKey = lift . Repo.getByKey
  insert = lift . Repo.insert
  insert' = lift . Repo.insert' @TagMappingEntity
  delete = lift . Repo.delete @TagMappingEntity
  update = lift . Repo.update
  update' = lift . Repo.update'

instance
  ( ReleaseRepository m,
    ReleaseArtistNameRepository m,
    ReleaseAggregate m,
    ArtistAggregate m,
    ArtistNameRepository m,
    ArtistRepository m,
    TrackAggregate m,
    TagMappingAggregate m,
    Logging m,
    MonadCatch m,
    PrimMonad m,
    MB.MusicBrainzService m
  ) =>
  ReleaseAggregate (TransformPreviewT m)
  where
  importReleases srcs = do
    $(logDebug) "Preview importReleases called"
    importReleasesImpl srcs
  getRelease ref = TransformPreviewT do
    $(logDebug) "Preview getRelease called"
    Release.getRelease ref

instance
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

instance Logging m => FileSystemWatcher (TransformPreviewT m) where
  startWatching _ _ = $(logDebug) "Preview startWarching TransformPreviewT"
  stopWatching _ = $(logDebug) "Preview stopWatching TransformPreviewT"
  lockPathsDuring _ m = do
    $(logDebug) "Preview lockPathsDuring TransformPreviewT"
    m

instance Logging m => FileSystemWatcher (VirtualArtistRepoT m) where
  startWatching _ _ = $(logDebug) "Preview startWarching VirtualArtistRepoT"
  stopWatching _ = $(logDebug) "Preview stopWatching VirtualArtistRepoT"
  lockPathsDuring _ m = do
    $(logDebug) "Preview lockPathsDuring VirtualArtistRepoT"
    m

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
    FileSystemWatcher m,
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
                Just updatedSrc -> pure $ mapLeft ConversionError $ tryFrom updatedSrc
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
  LiteralPattern p -> pure $ Just p
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

parseMovePattern :: Text -> Either (Maybe (ParseErrorBundle Text Void)) (NonEmpty SourcePathPattern)
parseMovePattern s = nonEmptyRight =<< mapLeft Just (parse terms "" s)
  where
    nonEmptyRight :: [SourcePathPattern] -> Either (Maybe (ParseErrorBundle Text Void)) (NonEmpty SourcePathPattern)
    nonEmptyRight (p : ps) = Right $ p :| ps
    nonEmptyRight [] = Left Nothing
    terms = someTill term eof
    term = try format <|> try group <|> try literal
    format = do
      void (char '%')
      padZero <- isn't _Nothing <$> optional (char '0')
      width <- fmap (: []) <$> optional digitChar
      mapping' <- mapping
      case width of
        Just width' ->
          pure $
            PrintfPattern ("%" <> (if padZero then "0" else ".") <> width' <> "s") $
              MappingPattern mapping'
        Nothing -> pure $ MappingPattern mapping'
    mapping = T.pack <$> some (letterChar <|> char '_') <?> "mapping"
    group = do
      void (char '[')
      someTill term (char ']') >>= \case
        (t : ts) -> pure $ GroupPattern (t :| ts)
        [] -> fail "no terms in group"
    literal =
      LiteralPattern
        <$> ( some
                ( satisfy @Void isAlphaNum
                    <|> satisfy @Void isSpace
                    <|> satisfy @Void (\c -> not (isReservedSymbol c) && isPunctuation c)
                )
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
   in uriToFilePath s.source & \case
        Just filePath ->
          Repo.getSingle @CollectionEntity collectionRef >>= \case
            Nothing -> pure $ Left (CollectionNotFound collectionRef)
            Just CollectionTable {root_uri} -> case uriToFilePath =<< parseURI (T.unpack root_uri) of
              Nothing -> pure $ Left UnsupportedSourceKind
              Just basePath -> do
                mappings <- V.fromList . Map.elems <$> getAllMappings
                srcPath <- renderSourcePath basePath s patterns
                let dest = addExtension srcPath (takeExtension filePath)
                lockPathsDuring (dest :| []) $ runExceptT do
                  mf <- openMetadataFile filePath >>= mapE MetadataTransformError
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
                  raw <- extractTrackTo cuefile dest >>= mapE MultiTrackError
                  $(logDebug) $ "Mappings: " <> showt mappings
                  let vc =
                        fromMaybe (F.metadataFactory @F.VorbisComments F.emptyTags) $
                          F.convert' F.vorbisCommentsId metadata mappings
                  $(logDebug) $ "Original metadata: " <> showt metadata
                  $(logDebug) $ "Converted metadata: " <> showt vc
                  let m = H.fromList [(F.vorbisCommentsId, vc)]
                  let metadataFile = raw & #metadata .~ m
                  mf <- writeMetadataFile metadataFile dest >>= mapE MetadataTransformError
                  srcs <- importSources (V.singleton (FileSource collectionRef mf))
                  case srcs V.!? 0 of
                    Just s -> pure s
                    Nothing -> throwE (ImportFailed (ImportSourceError Nothing))
        Nothing -> pure $ Right s
extractTrack _ _ src = pure $ Right src

mapE :: Monad m => (b -> c) -> Either b a -> ExceptT c m a
mapE e = except . mapLeft e

data MetadataTransformation
  = SetMapping Text (Vector Text)
  | RemoveMappings (Vector Text)
  | RetainMappings (Vector Text)
  | AddTag {key :: Text, value :: Text}
  | RemoveTag {key :: Text, value :: Text}
  | RemoveTags {key :: Text}
  | RemoveAll
  deriving (Show)
  deriving TextShow via FromStringShow MetadataTransformation

editMetadata :: MonadSourceTransform m => Vector MetadataTransformation -> Source -> m (Either TransformationError Source)
editMetadata _ src@Source {metadata = Nothing} = pure $ Right src
editMetadata ts src = runExceptT do
  src' <- foldM (\src t -> ExceptT $ editMetadata' t src) src ts
  if src' /= src then
    updateSource src' >>= mapE ImportFailed
  else pure src'
  where
  editMetadata' _ src@Source {metadata = Nothing} = pure $ Right src
  editMetadata' (SetMapping mappingName vs) src@Source {metadata = Just metadata'} =
    runExceptT @TransformationError do
      mapping <- getMappingNamed mappingName >>= eitherToError . maybeToRight (UnknownTagMapping mappingName)
      let metadata = into @SourceMetadata metadata'.tags
          current  = metadata' ^. F.tagLens mapping in
        $(logDebugV ['mapping, 'metadata, 'current]) $ "Setting tag to " <> showt vs
      let newMetadata = metadata' & F.tagLens mapping .~ vs
      pure (src & #metadata .~ Just newMetadata)
  editMetadata' (RemoveMappings mappings) src = flatten <$> (forM mappings $ \mapping -> editMetadata' (SetMapping mapping V.empty) src)
    where
      flatten es = foldl' (\_a b -> b) (Left $ ImportFailed undefined) es
  editMetadata' (RetainMappings retained) src@Source {metadata = Just metadata} =
    let tag = F.mappedTag metadata.mappingSelector
     in runExceptT @TransformationError do
          retainedMappings <- forM retained $ \mappingName -> do
            getMappingNamed mappingName
              >>= eitherToError . maybeToRight (UnknownTagMapping mappingName)
          let newTags = foldl' (\ts mapping -> ts & tag mapping .~ (metadata.tag mapping)) F.emptyTags retainedMappings
          let newMetadata = metadata {F.tags = newTags}
          pure (src & #metadata .~ Just newMetadata)
  editMetadata' (AddTag k v) src@Source {metadata = Just metadata} = runExceptT @TransformationError do
    let (F.Tags tags) = metadata.tags
    let newMetadata = metadata {F.tags = F.Tags (V.snoc tags (k, v))}
    pure (src & #metadata .~ Just newMetadata)
  editMetadata' (RemoveTag k v) src@Source {metadata = Just metadata} = runExceptT @TransformationError do
    let (F.Tags tags) = metadata.tags
    let newMetadata = metadata {F.tags = F.Tags (V.filter (\(k', v') -> k' /= k && v' /= v) tags)}
    pure (src & #metadata .~ Just newMetadata)
  editMetadata' (RemoveTags k) src@Source {metadata = Just metadata} = runExceptT @TransformationError do
    let (F.Tags tags) = metadata.tags
    let newMetadata = metadata {F.tags = F.Tags (V.filter (\(k', _) -> k' /= k) tags)}
    pure (src & #metadata .~ Just newMetadata)
  editMetadata' RemoveAll src@Source {metadata = Just metadata} = runExceptT @TransformationError do
    let newMetadata = metadata {F.tags = F.Tags V.empty}
    pure (src & #metadata .~ Just newMetadata)

musicBrainzLookup :: MonadSourceTransform m => Source -> m (Either TransformationError Source)
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
  mapLeft ImportFailed <$> updateSource (src & #metadata .~ Just newMetadata)

copyCoverImage :: MonadSourceTransform m => URI -> Source -> m (Either TransformationError Source)
copyCoverImage imageUrl src =
  case uriToFilePath src.source of
    Just srcPath -> do
      Covers.copyCoverToDir imageUrl (takeDirectory srcPath)
      pure (Right src)
    Nothing -> pure (Right src)

convertMetadataFormat :: MonadSourceTransform m => F.MetadataId -> Source -> m (Either TransformationError Source)
convertMetadataFormat mid src = do
  !mappings <- V.fromList . Map.elems <$> getAllMappings

  case flip (F.convert' mid) mappings =<< src.metadata of
    Just metadata -> pure $ Right (src & #metadata ?~ metadata)
    Nothing -> pure $ Left $ MetadataConversionError (fromMaybe nullMetadata $ src.metadata <&> (.formatId)) mid
