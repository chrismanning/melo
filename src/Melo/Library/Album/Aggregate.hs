{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Album.Aggregate where

import Control.Exception.Safe
import Control.Foldl (impurely, vectorM)
import Control.Lens hiding (from, lens)
import Data.Vector (Vector (), (!))
import Data.Vector qualified as V
import Melo.Common.Logging
import Melo.Common.Monad
import Melo.Database.Repo as Repo
import Melo.Library.Album.ArtistName.Repo (AlbumArtistNameRepository)
import Melo.Library.Album.ArtistName.Repo qualified as AlbumArtist
import Melo.Library.Album.ArtistName.Types
import Melo.Library.Album.Repo as Album
import Melo.Library.Album.Types
import Melo.Library.Artist.Aggregate
import Melo.Library.Artist.Name.Repo
import Melo.Library.Artist.Name.Types
import Melo.Library.Artist.Repo
import Melo.Library.Artist.Types
import Melo.Library.Source.Types
import Melo.Library.Track.Aggregate
import Melo.Lookup.MusicBrainz qualified as MB
import Melo.Metadata.Mapping.Aggregate
import Streaming.Prelude qualified as S
import Witch

class Monad m => AlbumAggregate m where
  importAlbums :: Vector Source -> m (Vector Album)

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTrans t,
    AlbumAggregate m
  ) =>
  AlbumAggregate (t m)
  where
  importAlbums = lift . importAlbums

newtype AlbumAggregateIOT m a = AlbumAggregateIOT
  { runAlbumAggregateIOT :: m a
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
  ( AlbumRepository m,
    AlbumArtistNameRepository m,
    ArtistAggregate m,
    ArtistNameRepository m,
    ArtistRepository m,
    TrackAggregate m,
    TagMappingAggregate m,
    Logging m,
    PrimMonad m,
    MB.MusicBrainzService m
  ) =>
  AlbumAggregate (AlbumAggregateIOT m)
  where
  importAlbums srcs =
    lift $
      S.each srcs
        & S.mapM (\src -> (src,) <$> MB.getReleaseGroupFromMetadata src.metadata)
        & S.groupBy (\(_, releaseA) (_, releaseB) -> releaseA == releaseB)
        & S.mapped (impurely S.foldM vectorM)
        & S.map (\srcs -> (snd (srcs ! 0), fst <$> srcs))
        & \s ->
          S.for
            s
            ( \(mbRelease, srcs) -> case mbRelease of
                Just mbRelease -> importMusicBrainzRelease mbRelease srcs
                Nothing -> importAlbumsFromMetadata srcs
            )
            & impurely S.foldM_ vectorM
    where
      importMusicBrainzRelease :: MB.ReleaseGroup -> Vector Source -> S.Stream (S.Of Album) m ()
      importMusicBrainzRelease mbRelease srcs = do
        let newAlbum = insertSingle @AlbumEntity (from mbRelease)
        album <- Album.getByMusicBrainzId mbRelease.id <<|>> newAlbum
        artistNames <- case mbRelease.artistCredit of
          Just as -> V.mapMaybeM importArtistCredit as
          _ -> pure V.empty
        case album of
          Just album -> do
            $(logInfo) $ "Found album '" <> album.title <> "'"
            let mk a = AlbumArtistNameTable {album_id = album.id, artist_name_id = a.id}
            _ <- AlbumArtist.insert' (mk <$> artistNames)
            let album' = mkAlbum (V.toList artistNames) album
            _tracks <- importAlbumTracks srcs album'
            S.yield album'
          Nothing -> pure ()
      importAlbumsFromMetadata srcs = do
        S.each srcs
          & S.mapMaybeM (\src -> fmap (src,) <$> singleMapping "album_title" src)
          & S.groupBy (\(_, albumTitleA) (_, albumTitleB) -> albumTitleA == albumTitleB)
          & S.mapped (impurely S.foldM vectorM)
          & S.map (\srcs -> (snd (srcs ! 0), fst <$> srcs))
          & S.mapMaybeM (uncurry importAlbumSources)
      importAlbumSources title srcs = do
        let src = srcs ! 0
        yearReleased <- singleMapping "original_release_year" src
        album <-
          insertSingle @AlbumEntity
            NewAlbum
              { title,
                yearReleased,
                comment = Nothing,
                musicbrainzId = Nothing
              }
        albumArtistNames <- importAlbumArtists src album
        case mkAlbum albumArtistNames <$> album of
          Just album' -> do
            $(logInfo) $ "Imported " <> show album'
            _tracks <- importAlbumTracks srcs album'
            pure (Just album')
          Nothing -> do
            $(logWarn) $ "No album found for source " <> show src.ref
            pure Nothing

      importAlbumArtists :: Source -> Maybe AlbumEntity -> m (Vector ArtistNameEntity)
      importAlbumArtists _ Nothing = pure V.empty
      importAlbumArtists src (Just album) = do
        let mbArtistIds = MB.MusicBrainzId <$> src.metadata.tag MB.albumArtistIdTag
        newArtists <- V.mapMaybeM MB.getArtist mbArtistIds
        artists <- forMaybeM newArtists \newArtist -> runMaybeT do
          artist <- MaybeT $ insertSingle @ArtistEntity (from newArtist)
          artistName <- MaybeT $ insertSingle @ArtistNameEntity (NewArtistName artist.id artist.name)
          _ <- MaybeT $ AlbumArtist.insertSingle (AlbumArtistNameEntity album.id artistName.id)
          pure artistName

        albumArtists <- resolveMappingNamed "album_artist" src
        if V.length artists < V.length albumArtists
          then do
            $(logInfo) $ "No album artist MusicBrainz info found for source " <> show src
            forMaybeM albumArtists \albumArtist -> runMaybeT do
              artist <- MaybeT $ insertSingle @ArtistEntity (mkNewArtist albumArtist)
              artistName <- MaybeT $ insertSingle @ArtistNameEntity (NewArtistName artist.id albumArtist)
              _ <- MaybeT $ AlbumArtist.insertSingle (AlbumArtistNameEntity album.id artistName.id)
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