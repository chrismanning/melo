{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Artist.Service where

import Control.Lens hiding (from, lens)
import Data.Maybe
import Data.Vector (fromList, singleton)
import Melo.Common.Logging
import Melo.Database.Repo
import Melo.Format.Mapping qualified as M
import Melo.Format.Metadata
import Melo.Library.Artist.Name.Repo
import Melo.Library.Artist.Name.Types
import Melo.Library.Artist.Repo
import Melo.Library.Artist.Types
import Melo.Lookup.MusicBrainz qualified as MB
import Melo.Lookup.MusicBrainz ((<<|>>))
import Witch

importArtistCredit ::
  ( MB.MusicBrainzService m,
    ArtistRepository m,
    ArtistNameRepository m,
    Logging m
  ) =>
  MB.ArtistCredit ->
  m (Maybe ArtistNameEntity)
importArtistCredit artistCredit = do
  getByMusicBrainzId artistCredit.artist.id <<|>> newArtist >>= \case
    Just artist ->
      case artistCredit.name of
        Just alias -> do
          artistName <- firstOf traverse <$> insert @ArtistNameEntity
            ( singleton
                NewArtistName
                  { artist = artist.id,
                    name = alias
                  }
            )
          pure artistName <<|>> getAlias artist.id alias
        Nothing -> pure Nothing
    Nothing -> do
      $(logError) $ "Unable to find artist in MusicBrainz with MBID " <> show artistCredit.artist.id
      pure Nothing
  where
    newArtist = do
      artist <- MB.getArtist artistCredit.artist.id <&> fromMaybe artistCredit.artist
      fmap (firstOf traverse) (insert (singleton $ from artist))

getArtistNamed ::
  ( MB.MusicBrainzService m,
    ArtistRepository m,
    ArtistNameRepository m,
    Logging m
  ) =>
  ArtistNameRef ->
  m (Maybe Artist)
getArtistNamed ref = do
  -- TODO getArtistNamed
  undefined

setArtists :: Metadata -> [Artist] -> Metadata
setArtists m a =
  let tag = lens m
      tags :: Tags = m.tags & tag M.trackArtistTag .~ (fromList a <&> (.name))
   in m { tags }
