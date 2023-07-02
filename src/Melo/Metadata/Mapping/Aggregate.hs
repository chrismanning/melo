{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Metadata.Mapping.Aggregate where

import Control.Foldl qualified as F
import Country
import Data.Map.Strict as Map
import Data.Vector qualified as V
import Melo.Common.Exception
import Melo.Common.Logging
import Melo.Common.Monad
import Melo.Database.Repo as Repo
import Melo.Format.Mapping qualified as M
import Melo.Library.Artist.Repo
import Melo.Library.Artist.Types
import Melo.Library.Source.Types
import Melo.Metadata.Mapping.Repo
import Melo.Metadata.Mapping.Types
import Streaming.Prelude qualified as S

class Monad m => TagMappingAggregate m where
  resolveMappingNamed :: Text -> Source -> m (Vector Text)
  getMappingNamed :: Text -> m (Maybe M.TagMapping)
  getMappingsNamed :: Vector Text -> m TagMappingIndex
  getAllMappings :: m TagMappingIndex

singleMapping :: TagMappingAggregate m => Text -> Source -> m (Maybe Text)
singleMapping m src = firstOf traverse <$> resolveMappingNamed m src

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTrans t,
    TagMappingAggregate m
  ) =>
  TagMappingAggregate (t m)
  where
  resolveMappingNamed n s = lift (resolveMappingNamed n s)
  getMappingNamed = lift . getMappingNamed
  getMappingsNamed = lift . getMappingsNamed
  getAllMappings = lift getAllMappings

type TagMappingIndex = Map Text M.TagMapping

newtype TagMappingAggregateT m a = TagMappingAggregateT
  { runTagMappingAggregateT :: ReaderT TagMappingIndex m a
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
      MonadReader TagMappingIndex,
      MonadThrow,
      MonadTrans,
      MonadTransControl,
      PrimMonad
    )

instance
  ( ArtistRepository m,
    TagMappingRepository m,
    Logging m
  ) =>
  TagMappingAggregate (TagMappingAggregateT m)
  where
  resolveMappingNamed m src | m == "release_artist_origin" = do
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
  getMappingNamed name = asks lookup
    where
      lookup :: TagMappingIndex -> Maybe M.TagMapping
      lookup mappings = mappings ^. at name
  getMappingsNamed names =
    S.each names
      & S.mapMaybeM (\m -> fmap (m,) <$> getMappingNamed m)
      & F.impurely S.foldM_ (F.generalize F.map)
  getAllMappings = ask

runTagMappingAggregate :: TagMappingRepository m => TagMappingAggregateT m a -> m a
runTagMappingAggregate (TagMappingAggregateT m) = do
  all <- getAll @TagMappingEntity
  let mappings = Map.fromList $ fmap (\e -> (e.name, from e.tagMapping)) $ V.toList all
  runReaderT m mappings

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
