{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Release.Aggregate where

import Control.Applicative
import Control.Exception.Safe
import Control.Foldl (impurely, vectorM)
import Control.Lens hiding (from, lens)
import Data.Maybe
import Data.Text qualified as T
import Data.Vector (Vector (), (!))
import Data.Vector qualified as V
import Melo.Common.Logging
import Melo.Common.Monad
import Melo.Database.Repo as Repo
import Melo.Format (tagLens)
import Melo.Library.Artist.Aggregate
import Melo.Library.Artist.Name.Repo
import Melo.Library.Artist.Name.Types
import Melo.Library.Artist.Repo as Artist
import Melo.Library.Artist.Types
import Melo.Library.Release.ArtistName.Repo (ReleaseArtistNameRepository)
import Melo.Library.Release.ArtistName.Repo qualified as ReleaseArtist
import Melo.Library.Release.ArtistName.Types
import Melo.Library.Release.Repo as Release
import Melo.Library.Release.Types
import Melo.Library.Source.Types
import Melo.Library.Track.Aggregate
import Melo.Lookup.MusicBrainz qualified as MB
import Melo.Metadata.Mapping.Aggregate
import Streaming.Prelude qualified as S
import Witch

class Monad m => ReleaseAggregate m where
  importReleases :: Vector Source -> m (Vector Release)
  getRelease :: ReleaseRef -> m (Maybe Release)

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTrans t,
    ReleaseAggregate m
  ) =>
  ReleaseAggregate (t m)
  where
  importReleases = lift . importReleases
  getRelease = lift . getRelease

newtype ReleaseAggregateIOT m a = ReleaseAggregateIOT
  { runReleaseAggregateIOT :: m a
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
  ( ReleaseRepository m,
    ReleaseArtistNameRepository m,
    ArtistAggregate m,
    ArtistNameRepository m,
    ArtistRepository m,
    TrackAggregate m,
    TagMappingAggregate m,
    Logging m,
    PrimMonad m,
    MB.MusicBrainzService m
  ) =>
  ReleaseAggregate (ReleaseAggregateIOT m)
  where
  importReleases = importReleasesImpl
  getRelease ref =
    Repo.getSingle @ReleaseEntity ref >>= \case
      Nothing -> pure Nothing
      Just e -> do
        releaseArtists <- getReleaseArtists e.id
        pure $ Just $ mkRelease (releaseArtists <&> snd) e

importReleasesImpl ::
  forall m.
  (
    ReleaseRepository m,
    ReleaseArtistNameRepository m,
    ArtistAggregate m,
    ArtistNameRepository m,
    ArtistRepository m,
    TrackAggregate m,
    TagMappingAggregate m,
    Logging m,
    PrimMonad m,
    MB.MusicBrainzService m
  ) =>
  Vector Source -> m (Vector Release)
importReleasesImpl srcs =
  S.each srcs
    & S.mapM mbLookup
    & S.groupBy (\(_, releaseA) (_, releaseB) -> releaseA == releaseB)
    & S.mapped (impurely S.foldM vectorM)
    & S.map (\srcs -> (snd (srcs ! 0), fst <$> srcs))
    & importRelease'
    & impurely S.foldM_ vectorM
    where
      mbLookup :: Source -> m (Source, (Maybe MB.ReleaseGroup, Maybe MB.Release))
      mbLookup src = case src.metadata of
        Nothing -> pure (src, (Nothing, Nothing))
        Just metadata -> (src,) <$> MB.getReleaseAndGroup metadata
      importRelease' s = S.for s
        ( \((mbReleaseGroup, mbRelease), srcs) -> case (mbReleaseGroup, mbRelease) of
            (Nothing, Nothing) -> importReleasesFromMetadata srcs
            (mbReleaseGroup, mbRelease) -> importMusicBrainzRelease mbReleaseGroup mbRelease srcs
        )
      importMusicBrainzRelease mbReleaseGroup mbRelease srcs = case fromMusicBrainz mbReleaseGroup mbRelease of
        Nothing -> pure ()
        Just newRelease -> do
          $(logDebug) $ "Importing musicbrainz release for sources: " <> show (srcs <&> (.ref))
          release <- fmap join (traverse Release.getByMusicBrainzId (mbRelease ^? _Just . #id))
            <<|>> fmap join (traverse Release.getByMusicBrainzId (mbReleaseGroup ^? _Just . #id))
            <<|>> insertSingle @ReleaseEntity newRelease
          let artistCredits = mbRelease ^? _Just . #artistCredit . _Just <|> mbReleaseGroup ^? _Just . #artistCredit . _Just
          $(logDebug) $ "Artist credits for release " <> T.pack (show newRelease.title) <> ": " <> T.pack (show artistCredits)
          artistNames <- case artistCredits of
            Just as -> V.mapMaybeM importArtistCredit as
            _ -> pure V.empty
          case release of
            Just release -> do
              $(logInfo) $ "Found release " <> T.pack (show release.title) <> " (id: " <> T.pack (show release.id) <> ")"
              let mk a = ReleaseArtistNameTable {release_id = release.id, artist_name_id = a.id}
              _ <- ReleaseArtist.insert' (mk <$> artistNames)
              let release' = mkRelease (V.toList artistNames) release
              _tracks <- importReleaseTracks srcs release'
              S.yield release'
            Nothing -> pure ()
      importReleasesFromMetadata srcs = do
        $(logDebug) $ "Importing releases for sources: " <> show (srcs <&> (.ref))
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
        case mkRelease releaseArtistNames <$> release of
          Just release' -> do
            $(logInfo) $ "Imported " <> show release'
            _tracks <- importReleaseTracks srcs release'
            pure (Just release')
          Nothing -> do
            $(logWarn) $ "No release found for source " <> show src.ref
            pure Nothing

      importReleaseArtists :: Source -> Maybe ReleaseEntity -> m (Vector ArtistNameEntity)
      importReleaseArtists _ Nothing = pure V.empty
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
            $(logInfo) $ "No release artist MusicBrainz info found for source " <> show src
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
