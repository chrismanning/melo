{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Album.Aggregate where

import Control.Exception.Safe
import Control.Foldl (PrimMonad, impurely, vectorM)
import Control.Lens hiding (from, lens)
import Control.Monad.Conc.Class
import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.Identity
import Data.Vector (Vector, catMaybes, (!))
import Data.Vector qualified as V
import Melo.Database.Repo
import Melo.Library.Album.ArtistName.Repo as AlbumArtist
import Melo.Library.Album.ArtistName.Types
import Melo.Library.Album.Repo as Album
import Melo.Library.Album.Types
import Melo.Library.Artist.Aggregate
import Melo.Library.Artist.Name.Types
import Melo.Library.Source.Types
import Melo.Library.Track.Aggregate
import Melo.Lookup.MusicBrainz ((<<|>>))
import Melo.Lookup.MusicBrainz qualified as MB
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
    TrackAggregate m,
    PrimMonad m,
    MB.MusicBrainzService m
  ) =>
 AlbumAggregate (AlbumAggregateIOT m) where
  importAlbums srcs = do
    S.each srcs
      & S.mapM (\src -> (src,) <$> MB.getReleaseGroupFromMetadata src.metadata)
      & S.groupBy (\(_, releaseA) (_, releaseB) -> releaseA == releaseB)
      & S.mapped (impurely S.foldM vectorM)
      & S.map (\srcs -> (snd (srcs ! 0), fst <$> srcs))
      & S.mapMaybe (\(mbRelease, srcs) -> (,srcs) <$> mbRelease)
      & S.mapM
        ( \(mbRelease, srcs) -> do
            let newAlbum = insertSingle @AlbumEntity (from mbRelease)
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
