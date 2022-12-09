{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Metadata.Mapping.Aggregate where

import Control.Exception.Safe
import Control.Foldl qualified as F
import Control.Lens
import Country
import Data.Coerce
import Data.Map.Strict as Map
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Melo.Common.Logging
import Melo.Common.Monad
import Melo.Database.Repo as Repo
import Melo.Format.Mapping as M
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
  resolveMappingNamed m src | m == "album_artist_origin" = do
    artists <- getSourceAlbumArtists src.ref
    pure $ V.take 1 $ V.mapMaybe ((\c -> c >>= decodeAlphaThree <&> alphaTwoUpper) . (.country) . fst) artists
  resolveMappingNamed m src | m == "va_track_artist" = do
    albumArtists <- resolveMappingNamed "album_artist" src
    trackArtists <- resolveMappingNamed "track_artist" src
    if trackArtists /= albumArtists then
      pure trackArtists
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
  getMappingsNamed names = S.each names
    & S.mapMaybeM (\m -> fmap (m,) <$> getMappingNamed m)
    & F.impurely S.foldM_ (F.generalize F.map)
  getAllMappings = ask

runTagMappingAggregate :: TagMappingRepository m => TagMappingAggregateT m a -> m a
runTagMappingAggregate (TagMappingAggregateT m) = do
  all <- getAll
  let mappings = Map.fromList $ fmap (\e -> (e.name, e.tagMapping)) $ V.toList all
  runReaderT m mappings

insertDefaultMappings :: (MonadCatch m, TagMappingRepository m) => m ()
insertDefaultMappings = void (insert' defaultMappings) `catchIO` (\_ -> pure ())

defaultMappings :: Vector NewTagMapping
defaultMappings =
  V.fromList
    [ NewTagMapping "album_artist" $ coerce M.albumArtist,
      NewTagMapping "track_artist" $ coerce M.artist,
      NewTagMapping "artist" $ coerce M.artist,
      NewTagMapping "track_title" $ coerce M.trackTitle,
      NewTagMapping "title" $ coerce M.trackTitle,
      NewTagMapping "album_title" $ coerce M.album,
      NewTagMapping "album" $ coerce M.album,
      NewTagMapping "track_number" $ coerce M.trackNumber,
      NewTagMapping "tracknumber" $ coerce M.trackNumber,
      NewTagMapping "genre" $ coerce M.genre,
      NewTagMapping "year" $ coerce M.year,
      NewTagMapping "release_year" $ coerce M.year,
      NewTagMapping "original_release_year" $ coerce M.originalReleaseYear,
      NewTagMapping "total_tracks" $ coerce M.totalTracks,
      NewTagMapping "totaltracks" $ coerce M.totalTracks,
      NewTagMapping "disc_number" $ coerce M.discNumberTag,
      NewTagMapping "disc" $ coerce M.discNumberTag,
      NewTagMapping "total_discs" $ coerce M.totalDiscs,
      NewTagMapping "totaldiscs" $ coerce M.totalDiscs,
      NewTagMapping "catalogue_number" $ coerce M.catalogueNumber,
      NewTagMapping "catalog_number" $ coerce M.catalogNumber
    ]
