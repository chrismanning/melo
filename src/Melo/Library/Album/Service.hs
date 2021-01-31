{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Album.Service where

import Basement.From
import Control.Lens hiding (from, lens)
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Country
import Data.Default
import Data.Foldable
import Data.Functor
import qualified Data.HashMap.Strict as H
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Traversable
import GHC.Generics (Generic, Generic1)
import Melo.Common.Effect
import Melo.Common.Logging
import qualified Melo.Database.Model as DB
import Melo.Format.Internal.Metadata
import qualified Melo.Format.Mapping as M
import Melo.Format.Metadata
import Melo.Library.Album.Repo
import Melo.Library.Album.Types
import Melo.Library.Source.Types
import qualified Melo.Lookup.MusicBrainz as MB

class Monad m => AlbumService m where
  importAlbums :: [Source] -> m [Album]

newtype AlbumServiceT m a = AlbumServiceT
  { runAlbumServiceT :: m a
  }
  deriving newtype (Functor, Applicative, Monad)
  deriving (MonadTrans, MonadTransControl) via IdentityT

instance
  ( AlbumRepository m,
    Logging m,
    MB.MusicBrainzService m
  ) =>
  AlbumService (AlbumServiceT m)
  where
  importAlbums ms = AlbumServiceT $ do
    mbReleases <- catMaybes <$> mapM (MB.getReleaseFromMetadata . (^. #metadata)) ms
    $(logDebugShow) mbReleases
    albums <- insertAlbums (fmap from mbReleases) >>= getAlbums
    pure $ fmap from albums

runAlbumServiceIO :: AlbumServiceT m a -> m a
runAlbumServiceIO = runAlbumServiceT
