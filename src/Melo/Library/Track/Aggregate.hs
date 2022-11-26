{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Track.Aggregate where

import Control.Applicative
import Control.Exception.Safe
import Control.Lens hiding (from, lens)
import Data.Attoparsec.Text
import Data.Foldable.Extra
import Data.Int (Int16)
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tuple.Extra
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Generics hiding (from)
import Melo.Common.Logging
import Melo.Common.Monad
import Melo.Common.Uri
import Melo.Database.Repo
import Melo.Format (tagLens)
import Melo.Format.Mapping qualified as M
import Melo.Library.Album.Types
import Melo.Library.Artist.Aggregate
import Melo.Library.Artist.Name.Repo
import Melo.Library.Artist.Name.Types
import Melo.Library.Artist.Repo as Artist
import Melo.Library.Artist.Types
import Melo.Library.Source.Types
import Melo.Library.Track.ArtistName.Repo (TrackArtistNameRepository)
import Melo.Library.Track.ArtistName.Repo qualified as TrackArtist
import Melo.Library.Track.ArtistName.Types
import Melo.Library.Track.Repo as Track
import Melo.Library.Track.Types
import Melo.Lookup.MusicBrainz as MB
import Melo.Metadata.Mapping.Aggregate
import Streaming qualified as S
import Streaming.Prelude qualified as S
import Witch

class Monad m => TrackAggregate m where
  importAlbumTracks :: Vector Source -> Album -> m ()

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
    ArtistRepository m,
    ArtistNameRepository m,
    TagMappingAggregate m,
    Logging m
  ) =>
  TrackAggregate (TrackAggregateIOT m)
  where
  importAlbumTracks srcs album =
    lift $
      S.zip (S.each srcs) (S.each srcs & S.mapM importSourceTrack)
        & S.mapMaybe (\(src, track) -> (src,) <$> track)
        & S.mapM (\(src, track) -> fmap (src,track,) <$> newTrackArtists src)
        & (\s -> S.for s S.each)
        & S.groupBy (\a b -> (thd3 a).name == (thd3 b).name)
        & S.mapsM_ importTrackArtistGroup
    where
      importSourceTrack :: Source -> m (Maybe TrackEntity)
      importSourceTrack src = do
        $(logDebug) $ "Importing track from source " <> show src.ref
        recording <- join <$> traverse MB.getRecordingFromMetadata src.metadata
        firstJustM (Track.getByMusicBrainzId . (.id)) recording >>= \case
          Just existing -> do
            $(logWarn) $ "Found track " <> show existing.id <> " for MusicBrainz recording " <> show existing.musicbrainz_id <> " from source " <> show src.ref
            -- TODO allow multiple editions of same track despite musicbrainz lookup
            pure Nothing
          Nothing -> insertSingle (mkNewTrack album.dbId ((.id) <$> recording) src)
      newTrackArtists :: Source -> m (Vector NewArtist)
      newTrackArtists src = do
        albumArtists <- resolveMappingNamed "album_artist" src
        trackArtists <- resolveMappingNamed "track_artist" src
        if albumArtists == trackArtists
          then do
            $(logDebug) $ "Track artists same as album artists for source " <> show src.ref
            pure V.empty
          else do
            let mbArtistIds = MB.MusicBrainzId <$> fromMaybe V.empty (src.metadata ^? _Just . tagLens MB.artistIdTag)
            existingArtists <- V.mapMaybeM (Artist.getByMusicBrainzId) mbArtistIds

            if V.null existingArtists
              then do
                mbArtists <- V.mapMaybeM MB.getArtist mbArtistIds
                if V.length mbArtists < V.length trackArtists
                  then do
                    $(logWarn) $ "Invalid track artist MusicBrainz info found for source " <> show src
                    pure $ mkNewArtist <$> trackArtists
                  else pure $ from <$> mbArtists
              else pure V.empty
      importTrackArtistGroup :: S.Stream (S.Of (Source, TrackEntity, NewArtist)) m x -> m x
      importTrackArtistGroup s =
        S.next s >>= \case
          Right ((src, track, newArtist), s) -> do
            artist <- insertSingle @ArtistEntity newArtist
            artistNames <-
              join <$> traverse MB.getRecordingFromMetadata src.metadata >>= \case
                Just recording -> do
                  $(logInfo) $ "Using MusicBrainz recording credits as track artists for source " <> show src
                  case recording.artistCredit of
                    Just as -> V.mapMaybeM importArtistCredit as
                    Nothing -> do
                      $(logWarn) $ "MusicBrainz recording " <> show recording.id <> " found but no artist credits for source " <> show src.ref
                      insertNewArtistName artist
                Nothing -> insertNewArtistName artist
            s
              & S.map snd3
              & S.cons track
              & S.mapM_ \track -> TrackArtist.insert (TrackArtistNameTable track.id . (.id) <$> artistNames)
          Left x -> pure x
      insertNewArtistName artist = case artist of
        Just artist -> insert @ArtistNameEntity (V.singleton $ NewArtistName artist.id artist.name)
        Nothing -> pure V.empty
      mkNewArtist name =
        NewArtist
          { name,
            country = Nothing,
            disambiguation = Nothing,
            bio = Nothing,
            shortBio = Nothing,
            musicBrainzId = Nothing
          }

mkNewTrack :: AlbumRef -> Maybe MB.MusicBrainzId -> Source -> NewTrack
mkNewTrack albumRef mbid src@Source {metadata = Nothing} =
  NewTrack
    { title = "",
      trackNumber = fromMaybe 0 $ uriToFilePath src.source >>= parseTrackNumberFromFileName,
      discNumber = Nothing,
      comment = Nothing,
      length = fromMaybe 0 $ src.length,
      sourceId = src.ref,
      albumId = albumRef,
      musicBrainzId = mbid
    }
mkNewTrack albumRef mbid src@Source{metadata = Just m} =
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
    trackTitle = m.tagHead M.trackTitle & fromMaybe ""
    trackNumber =
      (m.tagHead M.trackNumber >>= parseTrackNumber)
        <|> (uriToFilePath src.source >>= parseTrackNumberFromFileName)
    trackComment = m.tagHead M.commentTag
    discNumber = case m.tagHead M.discNumberTag of
      Just dnum -> parseDiscNumber dnum
      _noDiscNum -> case m.tagHead M.trackNumber of
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
