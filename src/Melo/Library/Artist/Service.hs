{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Artist.Service where

import Basement.From
import Control.Algebra
import Control.Lens hiding (from, lens)
import Data.Foldable
import Data.Vector (fromList)
import Melo.Common.Logging
import qualified Melo.Database.Model as DB
import qualified Melo.Format.Mapping as M
import Melo.Format.Metadata
import Melo.Library.Artist.Repo
import Melo.Library.Artist.Staging.Repo
import Melo.Library.Artist.Types
import Melo.Library.Source.Types
import qualified Melo.Lookup.MusicBrainz as MB

importArtists ::
  ( Has MB.MusicBrainzService sig m,
    Has ArtistStagingRepository sig m,
    Has Logging sig m
  ) =>
  [Source] ->
  m [StagedArtist]
importArtists ss = do
  mbArtists <- fold <$> mapM (MB.getArtistFromMetadata . (^. #metadata)) ss
  $(logDebugShow) mbArtists
  artists <- insertStagedArtists (fmap from mbArtists) >>= getStagedArtists
  pure $ fmap from artists

-- TODO search Discogs (https://www.discogs.com/developers/#page:database)
-- TODO search Spotify (https://developer.spotify.com/documentation/web-api/reference/search/search/)
-- TODO search Rovi (http://developer.rovicorp.com/docs)

--commitArtists ::
--  ( Has ArtistStagingRepository sig m,
--    Has ArtistRepository sig m,
--    Has Logging sig m
--  ) => [DB.ArtistStageKey] -> m [DB.ArtistKey]
--
--mergeArtists ::
--  ( Has ArtistRepository sig m,
--    Has Logging sig m
--  ) =>
--  [DB.ArtistKey] -> DB.ArtistT x -> m DB.ArtistKey

setArtists :: Metadata -> [Artist] -> Metadata
setArtists m a =
  let tag = lens m
      ts = m ^. #tags
   in m {tags = ts & tag M.trackArtistTag .~ (fromList a <&> (^. #name))}
