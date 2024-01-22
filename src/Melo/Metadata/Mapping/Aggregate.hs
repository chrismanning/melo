{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Metadata.Mapping.Aggregate where

import Control.Foldl qualified as F
import Control.Monad.State.Strict
import Country
import Data.HashMap.Strict as HashMap
import Data.Time.Clock
import Data.Vector qualified as V
import Melo.Common.Exception
import Melo.Common.Monad
import Melo.Common.Tracing
import Melo.Database.Repo as Repo
import Melo.Format.Mapping qualified as M
import Melo.Library.Artist.Repo
import Melo.Library.Artist.Types
import Melo.Library.Source.Types
import Melo.Metadata.Mapping.Repo
import Melo.Metadata.Mapping.Types
import Streaming.Prelude qualified as S

class (Monad m) => TagMappingAggregate m where
  resolveMappingNamed :: Text -> Source -> m (Vector Text)
  getMappingNamed :: Text -> m (Maybe M.TagMapping)
  getMappingsNamed :: Vector Text -> m TagMappingIndex
  getAllMappings :: m TagMappingIndex

instance (TagMappingAggregate m) => TagMappingAggregate (StateT s m) where
  resolveMappingNamed m s = lift (resolveMappingNamed m s)
  getMappingNamed = lift . getMappingNamed
  getMappingsNamed = lift . getMappingsNamed
  getAllMappings = lift getAllMappings

singleMapping :: (TagMappingAggregate m) => Text -> Source -> m (Maybe Text)
singleMapping m src = firstOf traverse <$> resolveMappingNamed m src

type TagMappingIndex = HashMap Text M.TagMapping

data TagMappingIndexWrapper = TagMappingIndexWrapper
  { index :: TagMappingIndex,
    lastUpdated :: UTCTime
  }
  deriving (Typeable)

getTagMappingIndex :: AppM IO IO TagMappingIndex
getTagMappingIndex =
  getAppData @TagMappingIndexWrapper >>= \case
    Just w -> do
      now <- liftIO getCurrentTime
      if now `diffUTCTime` w.lastUpdated > secondsToNominalDiffTime 300
        then updateMappings
        else pure w.index
    Nothing -> updateMappings
  where
    updateMappings = do
      all <- getAll @TagMappingEntity
      let !mappings = HashMap.fromList $ fmap (\e -> (e.name, from e.tagMapping)) $ V.toList all
      now <- liftIO getCurrentTime
      putAppData (TagMappingIndexWrapper mappings now)
      pure mappings

instance TagMappingAggregate (AppM IO IO) where
  resolveMappingNamed m src | m == "release_artist_origin" = withSpan "resolveMappingNamed$release_artist_origin" defaultSpanArguments do
    artists <- getSourceReleaseArtists src.ref
    pure $ V.take 1 $ V.mapMaybe ((\c -> c >>= decodeAlphaThree <&> alphaTwoUpper) . (.country) . fst) artists
  resolveMappingNamed m src | m == "va_track_artist" = do
    releaseArtists <- resolveMappingNamed "release_artist" src
    trackArtists <- resolveMappingNamed "track_artist" src
    if trackArtists /= releaseArtists
      then pure trackArtists
      else pure V.empty
  resolveMappingNamed _ Source {metadata = Nothing} = pure V.empty
  resolveMappingNamed mappingName Source {metadata = Just metadata} =
    getMappingNamed mappingName >>= \case
      Just mapping -> pure $ metadata.tag mapping
      Nothing -> pure V.empty
  getMappingNamed name = lookup <$> getTagMappingIndex
    where
      lookup :: TagMappingIndex -> Maybe M.TagMapping
      lookup mappings = mappings ^. at name
  getMappingsNamed names =
    S.each names
      & S.mapMaybeM (\m -> fmap (m,) <$> getMappingNamed m)
      & F.impurely S.foldM_ (F.generalize F.hashMap)
  getAllMappings = getTagMappingIndex

insertDefaultMappings :: (MonadCatch m, TagMappingRepository m) => m ()
insertDefaultMappings = void (insert' @TagMappingEntity defaultMappings) `catchIO` (\_ -> pure ())

defaultMappings :: Vector TagMapping
defaultMappings =
  V.fromList
    [ fromTagMapping "release_artist" M.albumArtist,
      fromTagMapping "track_artist" M.artist,
      fromTagMapping "artist" M.artist,
      fromTagMapping "track_title" M.trackTitle,
      fromTagMapping "title" M.trackTitle,
      fromTagMapping "release_title" M.album,
      fromTagMapping "album" M.album,
      fromTagMapping "track_number" M.trackNumber,
      fromTagMapping "tracknumber" M.trackNumber,
      fromTagMapping "genre" M.genre,
      fromTagMapping "year" M.year,
      fromTagMapping "release_year" M.year,
      fromTagMapping "original_release_year" M.originalReleaseYear,
      fromTagMapping "total_tracks" M.totalTracks,
      fromTagMapping "totaltracks" M.totalTracks,
      fromTagMapping "disc_number" M.discNumberTag,
      fromTagMapping "disc" M.discNumberTag,
      fromTagMapping "total_discs" M.totalDiscs,
      fromTagMapping "totaldiscs" M.totalDiscs,
      fromTagMapping "catalogue_number" M.catalogueNumber,
      fromTagMapping "catalog_number" M.catalogNumber
    ]
