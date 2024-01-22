module Melo.Library.Release.Aggregate where

import Control.Applicative
import Control.Foldl (impurely, vectorM)
import Control.Monad.State.Strict
import Data.Vector ((!))
import Data.Vector qualified as V
import Melo.Common.Exception
import Melo.Common.Logging
import Melo.Common.Monad
import Melo.Common.Tracing
import Melo.Database.Repo as Repo
import Melo.Database.Repo.IO
import Melo.Format (tagLens)
import Melo.Format.Mapping qualified as TagMapping
import Melo.Library.Artist.Aggregate
import Melo.Library.Artist.Name.Repo
import Melo.Library.Artist.Name.Types
import Melo.Library.Artist.Repo as Artist
import Melo.Library.Artist.Types
import Melo.Library.Genre.Repo
import Melo.Library.Genre.Types
import Melo.Library.Release.ArtistName.Repo (ReleaseArtistNameRepository)
import Melo.Library.Release.ArtistName.Repo qualified as ReleaseArtist
import Melo.Library.Release.ArtistName.Types
import Melo.Library.Release.Repo as Release
import Melo.Library.Release.Types
import Melo.Library.Source.Types
import Melo.Library.Track.Aggregate
import Melo.Lookup.MusicBrainz qualified as MB
import Melo.Metadata.Mapping.Aggregate
import Rel8 (in_)
import Rel8 qualified
import Streaming.Prelude qualified as S

class (Monad m) => ReleaseAggregate m where
  importReleases :: Vector Source -> m (Vector Release)
  getRelease :: ReleaseRef -> m (Maybe Release)

instance (ReleaseAggregate m) => ReleaseAggregate (StateT s m) where
  importReleases = lift . importReleases
  getRelease = lift . getRelease

instance ReleaseAggregate (AppM IO IO) where
  importReleases = importReleasesImpl
  getRelease ref =
    Repo.getSingle @ReleaseEntity ref >>= \case
      Nothing -> pure Nothing
      Just e -> do
        releaseArtists <- getReleaseArtists e.id
        releaseGenres <- runSelectM (genreForRelease (Rel8.lit e.id))
        pure $ Just $ mkRelease (releaseArtists <&> snd) releaseGenres e

importReleasesImpl ::
  forall m.
  ( ReleaseRepository m,
    ReleaseArtistNameRepository m,
    ArtistAggregate m,
    ArtistNameRepository m,
    ArtistRepository m,
    ReleaseGenreRepository m,
    TrackAggregate m,
    TagMappingAggregate m,
    Logging m,
    MonadCatch m,
    MonadIO m,
    PrimMonad m,
    MB.MusicBrainzService m,
    AppDataReader m,
    Tracing m
  ) =>
  Vector Source ->
  m (Vector Release)
importReleasesImpl srcs =
  S.each srcs
    & S.mapM mbLookup
    & S.groupBy (\(_, releaseA) (_, releaseB) -> releaseA == releaseB)
    & S.mapped (impurely S.foldM vectorM)
    & S.map (\srcs -> (snd (srcs ! 0), fst <$> srcs))
    & importRelease'
    & impurely S.foldM_ vectorM
    & handleException
  where
    handleException =
      handleAny \e -> do
        let cause = displayException e
        let sources = srcs <&> (showt . (.ref))
        $(logErrorV ['sources, 'cause]) "failed to import releases from sources"
        pure mempty
    mbLookup :: Source -> m (Source, (Maybe MB.ReleaseGroup, Maybe MB.Release))
    mbLookup src = case src.metadata of
      Nothing -> pure (src, (Nothing, Nothing))
      Just metadata -> (src,) <$> MB.getReleaseAndGroup metadata
    importRelease' s =
      S.for
        s
        ( \((mbReleaseGroup, mbRelease), srcs) -> case (mbReleaseGroup, mbRelease) of
            (Nothing, Nothing) -> importReleasesFromMetadata srcs
            (mbReleaseGroup, mbRelease) -> importMusicBrainzRelease mbReleaseGroup mbRelease srcs
        )
    importMusicBrainzRelease :: Maybe MB.ReleaseGroup -> Maybe MB.Release -> Vector Source -> S.Stream (S.Of Release) m ()
    importMusicBrainzRelease mbReleaseGroup mbRelease srcs = case fromMusicBrainz mbReleaseGroup mbRelease of
      Nothing -> pure ()
      Just newRelease -> handleAny
        ( \e -> do
            let cause = displayException e
            let sources = fmap (showt . (.ref)) srcs
            $(logErrorV ['sources, 'cause, 'newRelease]) "failed to import MusicBrainz release"
            pure mempty
        )
        do
          $(logDebug) $ "Importing musicbrainz release for sources: " <> showt (srcs <&> (.ref))
          release <-
            lift $
              fmap join (traverse Release.getByMusicBrainzId (mbRelease ^? _Just . #id))
                <<|>> fmap join (traverse Release.getByMusicBrainzId (mbReleaseGroup ^? _Just . #id))
                <<|>> insertSingle @ReleaseEntity newRelease
          let artistCredits = mbRelease ^? _Just . #artistCredit . _Just <|> mbReleaseGroup ^? _Just . #artistCredit . _Just
          $(logDebug) $ "Artist credits for release " <> showt newRelease.title <> ": " <> showt artistCredits
          artistNames <- lift case artistCredits of
            Just as -> V.mapMaybeM importArtistCredit as
            _ -> pure V.empty
          case release of
            Just release -> do
              $(logInfo) $ "Found release " <> showt release.title <> " (id: " <> showt release.id <> ")"
              let mk a = ReleaseArtistNameTable {release_id = release.id, artist_name_id = a.id}
              _ <- lift $ ReleaseArtist.insert' (mk <$> artistNames)
              releaseGenres <- lift $ runSelectM (genreForRelease (Rel8.lit release.id))
              let release' = mkRelease artistNames releaseGenres release
              _tracks <- lift $ importReleaseTracks srcs release'
              S.yield release'
            Nothing -> pure ()
    importReleasesFromMetadata :: Vector Source -> S.Stream (S.Of Release) m ()
    importReleasesFromMetadata srcs = handleAny
      ( \e -> do
          let cause = displayException e
          let sources = fmap (showt . (.ref)) srcs
          $(logErrorV ['sources, 'cause]) "failed to import release from metadata"
          pure mempty
      )
      do
        $(logDebug) $ "Importing releases for sources: " <> showt (srcs <&> (.ref))
        S.each srcs
          & S.mapMaybeM (\src -> fmap (src,) <$> singleMapping "release_title" src)
          & S.groupBy (\(_, releaseTitleA) (_, releaseTitleB) -> releaseTitleA == releaseTitleB)
          & S.mapped (impurely S.foldM vectorM)
          & S.map (\srcs -> (snd (srcs ! 0), fst <$> srcs))
          & S.mapMaybeM (uncurry importReleaseSources)
    importReleaseSources title srcs = do
      let src = srcs ! 0
      originalYearReleased <- singleMapping "original_release_year" src
      yearReleased <- singleMapping "release_year" src
      catalogueNumber <- singleMapping "catalogue_number" src
      release <-
        insertSingle @ReleaseEntity
          NewRelease
            { title,
              yearReleased,
              originalYearReleased,
              comment = Nothing,
              musicbrainzId = Nothing,
              musicbrainzGroupId = Nothing,
              kind = AlbumKind,
              catalogueNumber
            }
      releaseArtistNames <- importReleaseArtists src release
      genres <- importReleaseGenres src release
      case mkRelease releaseArtistNames genres <$> release of
        Just release' -> do
          $(logInfo) $ "Imported " <> showt release'
          _tracks <- importReleaseTracks srcs release'
          pure (Just release')
        Nothing -> do
          $(logWarn) $ "No release found for source " <> showt src.ref
          pure Nothing

    importReleaseArtists :: Source -> Maybe ReleaseEntity -> m (Vector ArtistNameEntity)
    importReleaseArtists _ Nothing = pure V.empty -- TODO update release artists when release already exists
    importReleaseArtists src (Just release) = do
      let mbArtistIds = MB.MusicBrainzId <$> fromMaybe V.empty (src.metadata ^? _Just . tagLens MB.albumArtistIdTag)
      newArtists <- V.mapMaybeM MB.getArtist mbArtistIds
      artists <- forMaybeM newArtists \newArtist -> runMaybeT do
        artist <- MaybeT $ insertSingle @ArtistEntity (from newArtist) <<|>> Artist.getByMusicBrainzId newArtist.id
        artistName <- MaybeT $ insertSingle @ArtistNameEntity (NewArtistName artist.id artist.name) <<|>> getAlias artist.id artist.name
        _ <- MaybeT $ ReleaseArtist.insertSingle (ReleaseArtistNameEntity release.id artistName.id)
        pure artistName

      releaseArtists <- resolveMappingNamed "release_artist" src
      if V.length artists < V.length releaseArtists
        then do
          $(logInfo) $ "No release artist MusicBrainz info found for source " <> showt src
          forMaybeM releaseArtists \releaseArtist -> runMaybeT do
            artist <- MaybeT $ insertSingle @ArtistEntity (mkNewArtist releaseArtist)
            artistName <- MaybeT $ insertSingle @ArtistNameEntity (NewArtistName artist.id releaseArtist)
            _ <- MaybeT $ ReleaseArtist.insertSingle (ReleaseArtistNameEntity release.id artistName.id)
            pure artistName
        else pure artists
    mkNewArtist name =
      NewArtist
        { name,
          country = Nothing,
          disambiguation = Nothing,
          bio = Nothing,
          shortBio = Nothing,
          musicBrainzId = Nothing
        }
    importReleaseGenres :: Source -> Maybe ReleaseEntity -> m (Vector GenreEntity)
    importReleaseGenres _ Nothing = pure mempty
    importReleaseGenres src (Just release) = do
      case src.metadata ^? _Just . tagLens TagMapping.genre of
        Just genreNames -> do
          $(logInfo) $ "Importing genres for release " <> showt release.id <> ": " <> showt genreNames
          delete @ReleaseGenreEntity (V.singleton release.id)
          genres <- runSelectM do
            genre <- Rel8.each genreSchema
            Rel8.where_ $
              genre.name `in_` (Rel8.lit <$> genreNames)
            pure genre
          insert' @ReleaseGenreEntity do
            genres <&> \genre -> ReleaseGenreTable release.id genre.id
          pure genres
        Nothing -> pure mempty

getReleasesForGenre :: GenreRef -> AppM IO IO (Vector Release)
getReleasesForGenre genre = do
  undefined
