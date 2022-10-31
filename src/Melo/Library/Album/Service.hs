module Melo.Library.Album.Service where

import Control.Foldl (PrimMonad, impurely, vectorM)
import Control.Lens hiding (from, lens)
import Data.Vector (Vector, catMaybes, (!))
import Data.Vector qualified as V
import Melo.Common.Logging
import Melo.Database.Repo
import Melo.Library.Album.ArtistName.Repo as AlbumArtist
import Melo.Library.Album.ArtistName.Types
import Melo.Library.Album.Repo as Album
import Melo.Library.Album.Types
import Melo.Library.Artist.Name.Repo
import Melo.Library.Artist.Name.Types
import Melo.Library.Artist.Repo
import Melo.Library.Artist.Service
import Melo.Library.Source.Types
import Melo.Library.Track.ArtistName.Repo
import Melo.Library.Track.Repo
import Melo.Library.Track.Service
import Melo.Lookup.MusicBrainz qualified as MB
import Melo.Lookup.MusicBrainz ((<<|>>))
import Streaming.Prelude qualified as S
import Witch

importAlbums ::
  ( AlbumRepository m,
    ArtistRepository m,
    ArtistNameRepository m,
    AlbumArtistNameRepository m,
    TrackRepository m,
    TrackArtistNameRepository m,
    Logging m,
    MB.MusicBrainzService m,
    PrimMonad m
  ) =>
  Vector Source ->
  m (Vector Album)
importAlbums srcs = do
  S.each srcs
    & S.mapM (\src -> (src,) <$> MB.getReleaseGroupFromMetadata src.metadata)
    & S.groupBy (\(_, releaseA) (_, releaseB) -> releaseA == releaseB)
    & S.mapped (impurely S.foldM vectorM)
    & S.map (\srcs -> (snd (srcs ! 0), fst <$> srcs))
    & S.mapMaybe (\(mbRelease, srcs) -> (,srcs) <$> mbRelease)
    & S.mapM
      ( \(mbRelease, srcs) -> do
          let newAlbum = firstOf traverse <$> insert @AlbumEntity (V.singleton $ from mbRelease)
          album <- Album.getByMusicBrainzId mbRelease.id <<|>> newAlbum
          artistNames <- case mbRelease.artistCredit of
            Just as -> catMaybes . V.fromList <$> mapM importArtistCredit as
            _ -> pure V.empty
          case album of
            Just album -> do
              let mk a = AlbumArtistNameTable {album_id = album.id, artist_name_id = a.id}
              _ <- AlbumArtist.insert' (mk <$> artistNames)
              let album' = mkAlbum (V.toList artistNames) album
              _tracks <- importAlbumTracks srcs album'
              pure (Just album')
            Nothing -> pure Nothing
      )
    & S.catMaybes
    & impurely S.foldM_ vectorM
