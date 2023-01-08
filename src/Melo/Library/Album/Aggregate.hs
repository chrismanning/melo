{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Album.Aggregate where

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
import Melo.Library.Album.ArtistName.Repo (AlbumArtistNameRepository)
import Melo.Library.Album.ArtistName.Repo qualified as AlbumArtist
import Melo.Library.Album.ArtistName.Types
import Melo.Library.Album.Repo as Album
import Melo.Library.Album.Types
import Melo.Library.Artist.Aggregate
import Melo.Library.Artist.Name.Repo
import Melo.Library.Artist.Name.Types
import Melo.Library.Artist.Repo as Artist
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
  importAlbums = importAlbumsImpl

importAlbumsImpl ::
  forall m.
  (
    AlbumRepository m,
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
  Vector Source -> m (Vector Album)
importAlbumsImpl srcs =
  S.each srcs
    & S.mapM mbLookup
    & S.groupBy (\(_, releaseA) (_, releaseB) -> releaseA == releaseB)
    & S.mapped (impurely S.foldM vectorM)
    & S.map (\srcs -> (snd (srcs ! 0), fst <$> srcs))
    & importAlbum'
    & impurely S.foldM_ vectorM
    where
      mbLookup :: Source -> m (Source, (Maybe MB.ReleaseGroup, Maybe MB.Release))
      mbLookup src = case src.metadata of
        Nothing -> pure (src, (Nothing, Nothing))
        Just metadata -> (src,) <$> MB.getReleaseAndGroup metadata
      importAlbum' s = S.for s
        ( \((mbReleaseGroup, mbRelease), srcs) -> case (mbReleaseGroup, mbRelease) of
            (Nothing, Nothing) -> importAlbumsFromMetadata srcs
            (mbReleaseGroup, mbRelease) -> importMusicBrainzRelease mbReleaseGroup mbRelease srcs
        )
      importMusicBrainzRelease mbReleaseGroup mbRelease srcs = case fromMusicBrainz mbReleaseGroup mbRelease of
        Nothing -> pure ()
        Just newAlbum -> do
          $(logDebug) $ "Importing musicbrainz release for sources: " <> show (srcs <&> (.ref))
          album <- fmap join (traverse Album.getByMusicBrainzId (mbRelease ^? _Just . #id))
            <<|>> fmap join (traverse Album.getByMusicBrainzId (mbReleaseGroup ^? _Just . #id))
            <<|>> insertSingle @AlbumEntity newAlbum
          let artistCredits = mbRelease ^? _Just . #artistCredit . _Just <|> mbReleaseGroup ^? _Just . #artistCredit . _Just
          $(logDebug) $ "Artist credits for album " <> T.pack (show newAlbum.title) <> ": " <> T.pack (show artistCredits)
          artistNames <- case artistCredits of
            Just as -> V.mapMaybeM importArtistCredit as
            _ -> pure V.empty
          case album of
            Just album -> do
              $(logInfo) $ "Found album " <> T.pack (show album.title) <> " (id: " <> T.pack (show album.id) <> ")"
              let mk a = AlbumArtistNameTable {album_id = album.id, artist_name_id = a.id}
              _ <- AlbumArtist.insert' (mk <$> artistNames)
              let album' = mkAlbum (V.toList artistNames) album
              _tracks <- importAlbumTracks srcs album'
              S.yield album'
            Nothing -> pure ()
      importAlbumsFromMetadata srcs = do
        $(logDebug) $ "Importing albums for sources: " <> show (srcs <&> (.ref))
        S.each srcs
          & S.mapMaybeM (\src -> fmap (src,) <$> singleMapping "album_title" src)
          & S.groupBy (\(_, albumTitleA) (_, albumTitleB) -> albumTitleA == albumTitleB)
          & S.mapped (impurely S.foldM vectorM)
          & S.map (\srcs -> (snd (srcs ! 0), fst <$> srcs))
          & S.mapMaybeM (uncurry importAlbumSources)
      importAlbumSources title srcs = do
        let src = srcs ! 0
        originalYearReleased <- singleMapping "original_release_year" src
        yearReleased <- singleMapping "release_year" src
        catalogueNumber <- singleMapping "catalogue_number" src
        album <-
          insertSingle @AlbumEntity
            NewAlbum
              { title,
                yearReleased,
                originalYearReleased,
                comment = Nothing,
                musicbrainzId = Nothing,
                musicbrainzGroupId = Nothing,
                catalogueNumber
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
        let mbArtistIds = MB.MusicBrainzId <$> fromMaybe V.empty (src.metadata ^? _Just . tagLens MB.albumArtistIdTag)
        newArtists <- V.mapMaybeM MB.getArtist mbArtistIds
        artists <- forMaybeM newArtists \newArtist -> runMaybeT do
          artist <- MaybeT $ insertSingle @ArtistEntity (from newArtist) <<|>> Artist.getByMusicBrainzId newArtist.id
          artistName <- MaybeT $ insertSingle @ArtistNameEntity (NewArtistName artist.id artist.name) <<|>> getAlias artist.id artist.name
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
