{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Track.Aggregate where

import Control.Applicative
import Control.Lens hiding (from, lens)
import Data.Attoparsec.Text
import Data.Foldable.Extra
import Data.Int (Int16)
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Generics hiding (from)
import Melo.Common.Exception
import Melo.Common.Logging
import Melo.Common.Monad
import Melo.Common.Uri
import Melo.Database.Repo
import Melo.Format (tagLens)
import Melo.Format.Mapping qualified as M
import Melo.Library.Artist.Aggregate
import Melo.Library.Artist.Name.Types
import Melo.Library.Artist.Repo as Artist
import Melo.Library.Artist.Types
import Melo.Library.Release.Types
import Melo.Library.Source.Types
import Melo.Library.Track.ArtistName.Repo (TrackArtistNameRepository)
import Melo.Library.Track.ArtistName.Repo qualified as TrackArtist
import Melo.Library.Track.ArtistName.Types
import Melo.Library.Track.Repo as Track
import Melo.Library.Track.Types
import Melo.Lookup.MusicBrainz qualified as MB
import Melo.Metadata.Mapping.Aggregate
import Streaming.Prelude qualified as S

class Monad m => TrackAggregate m where
  importReleaseTracks :: Vector Source -> Release -> m ()

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTrans t,
    TrackAggregate m
  ) =>
  TrackAggregate (t m)
  where
  importReleaseTracks ss a = lift (importReleaseTracks ss a)

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
    TagMappingAggregate m,
    Logging m
  ) =>
  TrackAggregate (TrackAggregateIOT m)
  where
  importReleaseTracks srcs release =
    lift $
      S.zip (S.each srcs) (S.each srcs & S.mapM importSourceTrack)
        & S.mapMaybe (\(src, track) -> (src,) <$> track)
        & S.mapM_ (\(src, track) -> linkTrackArtists src track)
    where
      importSourceTrack :: Source -> m (Maybe TrackEntity)
      importSourceTrack src = do
        $(logDebug) $ "Importing track from source " <> show src.ref
        Track.getBySrcRef src.ref >>= \case
          Just track -> updateSingle (track {release_id = release.ref})
          Nothing -> insertSingle (mkNewTrack release.ref Nothing src)
      linkTrackArtists :: Source -> TrackEntity -> m ()
      linkTrackArtists src track = do
        releaseArtists <- resolveMappingNamed "release_artist" src
        trackArtists <- resolveMappingNamed "track_artist" src
        if releaseArtists == trackArtists
          then do
            $(logDebug) $ "Track artists same as release artists for source " <> show src.ref
            releaseArtists <- getReleaseArtists release.ref
            let artistNames = releaseArtists <&> (^. _2 . #id)
            void $ TrackArtist.insert' (TrackArtistNameTable track.id <$> artistNames)
          else do
            $(logDebug) $ "Track artists differ from release artists for source " <> show src.ref
            unlessM (importArtistsByRecording src track) do
              let mbArtistIds = MB.MusicBrainzId <$> fromMaybe V.empty (src.metadata ^? _Just . tagLens MB.artistIdTag)
              importArtistsByMetadata src track trackArtists mbArtistIds
      importArtistsByMetadata src _track trackArtists mbArtistIds
        | V.length mbArtistIds /= V.length trackArtists =
            $(logWarn) $ "Invalid track artist MusicBrainz info found for source " <> show src
      importArtistsByMetadata _src track trackArtists mbArtistIds =
        V.forM_ mbArtistIds $ \mbid ->
          MB.getArtist mbid >>= \case
            Just mbArtist ->
              importMusicBrainzArtist mbArtist >>= \case
                Just (artist, names) ->
                  forM_ trackArtists \trackArtist ->
                    case V.find (\n -> n.name == trackArtist) names of
                      Just artistName ->
                        void $ TrackArtist.insertSingle (TrackArtistNameTable track.id artistName.id)
                      Nothing ->
                        $(logWarn) $ "No artist name matching " <> show trackArtist <> " for artist " <> show artist.id
                Nothing -> $(logWarn) ("No artist imported" :: String)
            Nothing ->
              $(logWarn) $ "No MusicBrainz artist found with MBID " <> show mbid.mbid
      importArtistsByRecording src track =
        join <$> traverse MB.getRecordingFromMetadata src.metadata >>= \case
          Just mbRecording ->
            case mbRecording.artistCredit of
              Just artistCredits -> do
                artistNames <- V.mapMaybeM importArtistCredit artistCredits
                TrackArtist.insert' (TrackArtistNameTable track.id . (.id) <$> artistNames)
                pure True
              Nothing -> do
                $(logDebug) $ "No artist credits for recording " <> show mbRecording.id.mbid
                pure False
          Nothing -> do
            $(logDebug) $ "No recording for track " <> show track.id
            pure False

mkNewTrack :: ReleaseRef -> Maybe MB.MusicBrainzId -> Source -> NewTrack
mkNewTrack releaseRef mbid src@Source {metadata = Nothing} =
  NewTrack
    { title = "",
      trackNumber = fromMaybe 0 $ uriToFilePath src.source >>= parseTrackNumberFromFileName,
      discNumber = Nothing,
      comment = Nothing,
      length = fromMaybe 0 $ src.length,
      sourceId = src.ref,
      releaseId = releaseRef,
      musicBrainzId = mbid
    }
mkNewTrack releaseRef mbid src@Source {metadata = Just m} =
  NewTrack
    { title = trackTitle,
      trackNumber = fromMaybe 0 trackNumber,
      discNumber = discNumber,
      comment = trackComment,
      length = fromMaybe 0 $ src.length,
      sourceId = src.ref,
      releaseId = releaseRef,
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
