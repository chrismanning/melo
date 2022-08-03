{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Album.Service where

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
import Data.Vector (Vector, fromList)
import GHC.Generics (Generic, Generic1)
import Melo.Common.Effect
import Melo.Common.Logging
import Melo.Database.Repo
import Melo.Format.Internal.Metadata
import qualified Melo.Format.Mapping as M
import Melo.Format.Metadata
import Melo.Library.Album.Repo
import Melo.Library.Album.Types
import Melo.Library.Source.Types
import qualified Melo.Lookup.MusicBrainz as MB
import Witch

class Monad m => AlbumService m where
  importAlbums :: [Source] -> m (Vector Album)

newtype AlbumServiceIOT m a = AlbumServiceIOT
  { runAlbumServiceIOT :: m a
  }
  deriving newtype (Functor, Applicative, Monad)
  deriving (MonadTrans, MonadTransControl) via IdentityT

instance
  ( AlbumRepository m,
    Logging m,
    MB.MusicBrainzService m
  ) =>
  AlbumService (AlbumServiceIOT m)
  where
  importAlbums ms = AlbumServiceIOT $ do
    mbReleases <- catMaybes <$> mapM (MB.getReleaseFromMetadata . (^. #metadata)) ms
    $(logDebugShow) mbReleases
    albums <- insert (fromList $ fmap from mbReleases) <&> fmap (^. #id) >>= getByKey
    pure $ fmap from albums

runAlbumServiceIO :: AlbumServiceIOT m a -> m a
runAlbumServiceIO = runAlbumServiceIOT
