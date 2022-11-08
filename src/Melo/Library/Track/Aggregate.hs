{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Track.Aggregate where

import Control.Applicative
import Control.Exception.Safe
import Control.Foldl (PrimMonad)
import Control.Lens hiding (from, lens)
import Control.Monad
import Control.Monad.Base
import Control.Monad.Conc.Class
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.Identity
import Data.Attoparsec.Text
import Data.Int (Int16)
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Generics hiding (from)
import Melo.Common.Logging
import Melo.Common.Uri
import Melo.Database.Repo
import Melo.Format.Internal.Metadata
import Melo.Format.Mapping qualified as M
import Melo.Library.Album.Types
import Melo.Library.Artist.Aggregate
import Melo.Library.Artist.Name.Types
import Melo.Library.Source.Types
import Melo.Library.Track.ArtistName.Repo as TrackArtist
import Melo.Library.Track.ArtistName.Types
import Melo.Library.Track.Repo as Track
import Melo.Library.Track.Types
import Melo.Lookup.MusicBrainz as MB

class Monad m => TrackAggregate m where
  importAlbumTracks :: Vector Source -> Album -> m (Vector Track)

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTrans t,
    TrackAggregate m
  ) =>
  TrackAggregate (t m)
  where
  importAlbumTracks ss a = lift (importAlbumTracks ss a)

newtype TrackAggregateIOT m a = TrackAggregateIOT
  { runTrackAggregateIOT :: m a
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
  ( TrackRepository m,
    TrackArtistNameRepository m,
    MB.MusicBrainzService m,
    ArtistAggregate m,
    Logging m
  ) =>
  TrackAggregate (TrackAggregateIOT m) where
  importAlbumTracks srcs album = do
    tracks <- forM srcs $ \src -> do
      $(logDebug) $ "Importing track from source " <> show src.ref
      MB.getRecordingFromMetadata src.metadata >>= \case
        Just recording -> do
          let newTrack = insertSingle (mkNewTrack album.dbId (Just recording.id) src)
          Track.getByMusicBrainzId recording.id <<|>> newTrack >>= \case
            Just track -> do
              artistNames <- case recording.artistCredit of
                Just as -> V.catMaybes . V.fromList <$> mapM importArtistCredit as
                Nothing -> pure V.empty
              let mk a = TrackArtistNameTable {track_id = track.id, artist_name_id = a.id}
              _ <- TrackArtist.insert' (mk <$> artistNames)
              pure (Just (mkTrack (V.toList artistNames) track))
            Nothing -> pure Nothing
        Nothing -> do
          -- TODO get track artists from tags
          --        let Metadata{lens, tags} = src.metadata
          --        let artists = tags ^. lens M.trackArtistTag
          --        album
          fmap (mkTrack []) <$> insertSingle (mkNewTrack album.dbId Nothing src)
    pure (V.catMaybes tracks)
    where
      a <<|>> b =
        a >>= \case
          Just a' -> pure (Just a')
          Nothing -> b

mkNewTrack :: AlbumRef -> Maybe MB.MusicBrainzId -> Source -> NewTrack
mkNewTrack albumRef mbid src =
  NewTrack
    { title = trackTitle,
      trackNumber = fromMaybe 0 trackNumber,
      discNumber = discNumber,
      comment = trackComment,
      length = fromMaybe 0 $ src.length,
      sourceId = src.ref,
      albumId = albumRef,
      musicBrainzId = mbid
    }
  where
    tags = src.metadata.tags
    Metadata {lens} = src.metadata
    trackTitle = fromMaybe "" (tags ^? lens M.trackTitle . _head)
    trackNumber =
      (tags ^? lens M.trackNumber . _head >>= parseTrackNumber)
        <|> (uriToFilePath src.source >>= parseTrackNumberFromFileName)
    trackComment = tags ^? lens M.commentTag . _head
    discNumber = case tags ^? lens M.discNumberTag . _head of
      Just dnum -> parseDiscNumber dnum
      _noDiscNum -> case tags ^? lens M.trackNumber . _head of
        Just tnum -> parseDiscNumberFromTrackNumber tnum
        _noTrackNum -> Nothing

data TrackNumber = TrackNumber
  { discNumber :: Maybe Int16,
    trackNumber :: Int16,
    totalTracks :: Maybe Int16
  }
  deriving (Generic)

trackNumParser :: Parser TrackNumber
trackNumParser = do
  skipSpace
  discNumber <- option Nothing (Just <$> decimal <* char '.')
  trackNumber <- skipSpace *> decimal <* skipSpace
  totalTracks <- option Nothing (Just <$> (char '/' >> skipSpace *> decimal))
  pure
    TrackNumber
      { discNumber,
        trackNumber,
        totalTracks
      }

--- parses track number tags of form "1", "01", "01/11", "1.01/11"
parseTrackNumber :: Text -> Maybe Int16
parseTrackNumber num = case parseOnly trackNumParser num of
  Right tn -> Just tn.trackNumber
  Left _ -> Nothing

parseTrackNumberFromFileName :: FilePath -> Maybe Int16
parseTrackNumberFromFileName = parseTrackNumber . T.pack

parseDiscNumber :: Text -> Maybe Int16
parseDiscNumber num = case parseOnly decimal num of
  Right d -> Just d
  Left _ -> Nothing

parseDiscNumberFromTrackNumber :: Text -> Maybe Int16
parseDiscNumberFromTrackNumber num = case parseOnly trackNumParser num of
  Right d -> d.discNumber
  Left _ -> Nothing
