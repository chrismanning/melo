{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Artist.Service where

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
import Data.Vector (fromList)
import GHC.Generics (Generic, Generic1)
import Melo.Common.Effect
import Melo.Common.Logging
import Melo.Format.Internal.Metadata
import qualified Melo.Format.Mapping as M
import Melo.Format.Metadata
import Melo.Library.Artist.Repo
import Melo.Library.Artist.Types
import qualified Melo.Library.Database.Model as DB
import Melo.Library.Source.Types
import qualified Melo.Lookup.MusicBrainz as MB

data ArtistService :: Effect where
  ImportArtists :: [Source] -> ArtistService m [Artist]

importArtists :: Has ArtistService sig m => [Source] -> m [Artist]
importArtists m = send (ImportArtists m)

newtype ArtistServiceIOC m a = ArtistServiceIOC
  { runArtistServiceIOC :: m a
  }
  deriving newtype (Applicative, Functor, Monad)

instance
  ( Has MB.MusicBrainzService sig m,
    Has ArtistRepository sig m,
    Has Logging sig m
  ) =>
  Algebra (ArtistService :+: sig) (ArtistServiceIOC m)
  where
  alg _hdl (L sig) ctx =
    case sig of
      ImportArtists ms -> do
        mbArtists <- fold <$> mapM (MB.getArtistFromMetadata . (^. #metadata)) ms
        $(logDebugShow) mbArtists
        artists <- insertArtists (fmap from mbArtists) >>= getArtists
        pure $ ctx $> fmap from artists
  -- TODO search Discogs (https://www.discogs.com/developers/#page:database)
  -- TODO search Spotify (https://developer.spotify.com/documentation/web-api/reference/search/search/)
  -- TODO search Rovi (http://developer.rovicorp.com/docs)
  alg hdl (R other) ctx = ArtistServiceIOC (alg (runArtistServiceIOC . hdl) other ctx)

runArtistServiceIO :: ArtistServiceIOC m a -> m a
runArtistServiceIO = runArtistServiceIOC

setArtists :: Metadata -> [Artist] -> Metadata
setArtists m a =
  let tag = lens m
      ts = m ^. #tags
   in m {tags = ts & tag M.trackArtistTag .~ (fromList a <&> (^. #name))}
