module Melo.Metadata.Mapping.Service where

import Control.Exception.Safe
import Control.Monad
import Data.Coerce
import Data.Text (Text)
import Data.Vector (Vector, find, fromList)
import Melo.Database.Repo as Repo
import Melo.Format.Mapping as M
import Melo.Metadata.Mapping.Repo
import Melo.Metadata.Mapping.Types

findMappingNamed :: Vector TagMappingEntity -> Text -> Maybe M.TagMapping
findMappingNamed ms name = fmap (\m -> m.tagMapping) $ find (\mapping -> mapping.name == name) ms

insertDefaultMappings :: (MonadCatch m, TagMappingRepository m) => m ()
insertDefaultMappings = void (insert' defaultMappings) `catchIO` (\_ -> pure ())

defaultMappings :: Vector NewTagMapping
defaultMappings =
  fromList
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
      NewTagMapping "totaldiscs" $ coerce M.totalDiscs
    ]
