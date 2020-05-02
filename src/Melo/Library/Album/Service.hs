{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Album.Service where

import Basement.From
import Control.Algebra
import Control.Carrier.Cull.Church
import Control.Carrier.NonDet.Church
import Control.Effect.Cull
import Control.Effect.Empty as E
import Control.Effect.NonDet
import Control.Effect.Reader
import Control.Effect.Sum
import Control.Lens hiding (from, lens)
import Control.Monad
import Control.Monad.IO.Class
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
import Melo.Format.Internal.Metadata
import qualified Melo.Format.Mapping as M
import Melo.Format.Metadata
import Melo.Library.Album.Repo
import Melo.Library.Album.Types
import qualified Melo.Library.Database.Model as DB
import Melo.Library.Source.Types
import qualified Melo.Lookup.MusicBrainz as MB

data AlbumService :: Effect where
  ImportAlbums :: [Source] -> AlbumService m [Album]

importAlbums :: Has AlbumService sig m => [Source] -> m [Album]
importAlbums m = send (ImportAlbums m)

newtype AlbumServiceIOC m a = AlbumServiceIOC
  { runAlbumServiceIOC :: m a
  }
  deriving newtype (Functor, Applicative, Monad)

instance
  ( Has AlbumRepository sig m,
    Has Logging sig m,
    Has MB.MusicBrainzService sig m
  ) =>
  Algebra (AlbumService :+: sig) (AlbumServiceIOC m)
  where
  alg _ (L sig) ctx = case sig of
    ImportAlbums ms -> do
      mbReleases <- catMaybes <$> mapM (MB.getReleaseFromMetadata . (^. #metadata)) ms
      $(logDebugShow) mbReleases
      albums <- insertAlbums (fmap from mbReleases) >>= getAlbums
      pure (ctx $> fmap from albums)
  alg hdl (R other) ctx = AlbumServiceIOC (alg (runAlbumServiceIOC . hdl) other ctx)

runAlbumServiceIO :: AlbumServiceIOC m a -> m a
runAlbumServiceIO = runAlbumServiceIOC
