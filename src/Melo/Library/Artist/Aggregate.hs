{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Artist.Aggregate where

import Control.Applicative
import Control.Monad.State.Strict
import Data.Vector qualified as V
import Melo.Common.Exception
import Melo.Common.Logging
import Melo.Common.Monad
import Melo.Database.Repo
import Melo.Library.Artist.Name.Repo
import Melo.Library.Artist.Name.Types
import Melo.Library.Artist.Repo
import Melo.Library.Artist.Types
import Melo.Lookup.MusicBrainz qualified as MB

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
    MonadCatch m,
    Logging m
  ) =>
  ArtistAggregate (ArtistAggregateIOT m)
  where
  importArtistCredit artistCredit = handleAny
    ( \e -> do
        let cause = displayException e
        $(logErrorV ['cause, 'artistCredit]) "failed to import artistCredit"
        pure Nothing
    )
    do
      mbArtist <- MB.getArtist artistCredit.artist.id <&> fromMaybe artistCredit.artist
      importMusicBrainzArtist mbArtist >>= \case
        Just (artist, names) ->
          case artistCredit.name of
            Just alias -> do
              artistName <-
                insertSingle @ArtistNameEntity
                  NewArtistName
                    { artist = artist.id,
                      name = alias
                    }
              pure (artistName <|> V.find (\a -> a.name == alias) names)
            Nothing -> getAlias artist.id artist.name
        Nothing -> do
          $(logError) $ "Unable to find MusicBrainz artist with MBID " <> showt artistCredit.artist.id.mbid
          pure Nothing
  importMusicBrainzArtist mbArtist =
    insertSingle @ArtistEntity (from mbArtist) <<|>> getByMusicBrainzId mbArtist.id >>= \case
      Just artist -> do
        case getAliases mbArtist.aliases of
          Just aliases -> do
            names <- insert @ArtistNameEntity (NewArtistName artist.id <$> aliases) <<|>> getArtistNames artist.id
            pure $ Just (artist, names)
          Nothing -> do
            names <- insert @ArtistNameEntity (V.singleton $ NewArtistName artist.id artist.name) <<|>> getArtistNames artist.id
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
