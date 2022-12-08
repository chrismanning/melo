{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Artist.Aggregate where

import Control.Exception.Safe
import Control.Lens hiding (from, lens)
import Control.Monad.State.Strict
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Generics hiding (from, to)
import Melo.Common.Logging
import Melo.Common.Monad
import Melo.Database.Repo
import Melo.Library.Artist.Name.Repo
import Melo.Library.Artist.Name.Types
import Melo.Library.Artist.Repo
import Melo.Library.Artist.Types
import Melo.Lookup.MusicBrainz qualified as MB
import Witch

class Monad m => ArtistAggregate m where
  importArtistCredit :: MB.ArtistCredit -> m (Maybe ArtistNameEntity)
  importMusicBrainzArtist :: MB.Artist -> m (Maybe (ArtistEntity, Vector ArtistNameEntity))

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTrans t,
    ArtistAggregate m
  ) =>
  ArtistAggregate (t m)
  where
  importArtistCredit = lift . importArtistCredit
  importMusicBrainzArtist = lift . importMusicBrainzArtist

newtype ArtistAggregateIOT m a = ArtistAggregateIOT
  { runArtistAggregateIOT :: m a
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
  ( MB.MusicBrainzService m,
    ArtistRepository m,
    ArtistNameRepository m,
    Logging m
  ) =>
  ArtistAggregate (ArtistAggregateIOT m)
  where
  importArtistCredit artistCredit = do
    newArtist >>= \case
      Just artist ->
        case artistCredit.name of
          Just alias -> do
            artistName <-
              insertSingle @ArtistNameEntity
                NewArtistName
                  { artist = artist.id,
                    name = alias
                  }
            void $ importMusicBrainzArtist artistCredit.artist
            pure artistName <<|>> getAlias artist.id alias
          Nothing -> do
            void $ importMusicBrainzArtist artistCredit.artist
            insertSingle @ArtistNameEntity
              NewArtistName
                { artist = artist.id,
                  name = artist.name
                }
      Nothing -> do
        $(logError) $ "Unable to find MusicBrainz artist with MBID " <> show artistCredit.artist.id.mbid
        pure Nothing
    where
      newArtist = do
        mbArtist <- MB.getArtist artistCredit.artist.id <&> fromMaybe artistCredit.artist
        insertSingle @ArtistEntity (from mbArtist) <<|>> getByMusicBrainzId mbArtist.id
  importMusicBrainzArtist mbArtist =
    insertSingle @ArtistEntity (from mbArtist) <<|>> getByMusicBrainzId mbArtist.id >>= \case
      Just artist -> do
        case getAliases mbArtist.aliases of
          Just aliases -> do
            names <- insert @ArtistNameEntity (NewArtistName artist.id <$> aliases)
            pure $ Just (artist, names)
          Nothing -> do
            names <- insert @ArtistNameEntity (V.singleton $ NewArtistName artist.id artist.name)
            pure $ Just (artist, names)
      Nothing -> pure Nothing
    where
      getAliases (Just aliases) = Just $ fmap (.name) $ V.filter (\a -> a.type' == Just "Artist Name") aliases
      getAliases _ = Nothing

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
