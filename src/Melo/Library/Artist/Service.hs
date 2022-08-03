{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Artist.Service where

import Control.Lens hiding (from, lens)
import Data.Foldable
import Data.Vector (Vector, fromList)
import Melo.Common.Logging
import Melo.Database.Repo
import Melo.Format.Mapping qualified as M
import Melo.Format.Metadata
import Melo.Library.Artist.Staging.Repo
import Melo.Library.Artist.Staging.Types
import Melo.Library.Artist.Types
import Melo.Library.Source.Types
import Melo.Lookup.MusicBrainz qualified as MB
import Witch

importArtists ::
  ( Foldable f,
    Traversable f,
    MB.MusicBrainzService m,
    ArtistStagingRepository m,
    Logging m
  ) =>
  f Source ->
  m (Vector StagedArtist)
importArtists ss = do
  mbArtists <- fold <$> mapM (MB.getArtistFromMetadata . (^. #metadata)) ss
  $(logDebugShow) mbArtists
  artists <- insert (fromList $ toList $ fmap from mbArtists) <&> fmap (^. #id) >>= getByKey
  pure $ fmap from artists

-- TODO search Discogs (https://www.discogs.com/developers/#page:database)
-- TODO search Spotify (https://developer.spotify.com/documentation/web-api/reference/search/search/)
-- TODO search Rovi (http://developer.rovicorp.com/docs)

-- commitArtists ::
--  ( Has ArtistStagingRepository sig m,
--    Has ArtistRepository sig m,
--    Has Logging sig m
--  ) => [StagedArtistRef] -> m [DB.ArtistKey]
--
-- mergeArtists ::
--  ( Has ArtistRepository sig m,
--    Has Logging sig m
--  ) =>
--  [DB.ArtistKey] -> DB.ArtistT x -> m DB.ArtistKey

setArtists :: Metadata -> [Artist] -> Metadata
setArtists m a =
  let tag = lens m
      ts = m.tags
   in m {tags = ts & tag M.trackArtistTag .~ (fromList a <&> (^. #name))}
