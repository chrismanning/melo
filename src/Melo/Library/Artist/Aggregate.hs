{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Artist.Aggregate where

import Control.Exception.Safe
import Control.Lens hiding (from, lens)
import Data.Maybe
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Melo.Common.Logging
import Melo.Common.Monad
import Melo.Database.Repo
import Melo.Library.Artist.Name.Repo
import Melo.Library.Artist.Name.Types
import Melo.Library.Artist.Repo
import Melo.Library.Artist.Types
import Melo.Library.Source.Types
import Melo.Library.Track.Types
import Melo.Lookup.MusicBrainz qualified as MB
import Witch

class Monad m => ArtistAggregate m where
  importArtistCredit :: MB.ArtistCredit -> m (Maybe ArtistNameEntity)

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTrans t,
    ArtistAggregate m
  ) =>
  ArtistAggregate (t m)
  where
  importArtistCredit = lift . importArtistCredit

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
    getByMusicBrainzId artistCredit.artist.id <<|>> newArtist >>= \case
      Just artist ->
        case artistCredit.name of
          Just alias -> do
            artistName <-
              insertSingle @ArtistNameEntity
                NewArtistName
                  { artist = artist.id,
                    name = alias
                  }
            pure artistName <<|>> getAlias artist.id alias
          Nothing -> pure Nothing
      Nothing -> do
        $(logError) $ "Unable to find artist in MusicBrainz with MBID " <> show artistCredit.artist.id
        pure Nothing
    where
      newArtist = do
        artist <- MB.getArtist artistCredit.artist.id <&> fromMaybe artistCredit.artist
        insertSingle (from artist)

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
