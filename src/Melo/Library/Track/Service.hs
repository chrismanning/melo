{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Track.Service where

import Control.Lens hiding (from, lens)
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Data.Attoparsec.Text
import Data.Foldable
import Data.Functor
import qualified Data.HashMap.Strict as H
import Data.Int (Int16)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Traversable
import Data.Vector (Vector, (!?))
import qualified Data.Vector as V
import GHC.Generics hiding (from)
import Melo.Common.Effect
import Melo.Common.Logging
import Melo.Common.Metadata
import Melo.Database.Repo
import Melo.Format.Info
import Melo.Format.Internal.Metadata
import qualified Melo.Format.Mapping as M
import Melo.Format.Metadata (TagLens)
import Melo.Library.Album.Repo
import Melo.Library.Album.Service
import Melo.Library.Album.Types
import Melo.Library.Artist.Repo
import Melo.Library.Artist.Service
import Melo.Library.Artist.Types
import Melo.Library.Source.Types
import Melo.Library.Track.Repo
import Melo.Library.Track.Types
import Network.URI
import System.Directory
import Witch

class Monad m => TrackService m where
  importTracks :: Vector Source -> m (Vector Track)

newtype TrackServiceIOT m a = TrackServiceIOT
  { runTrackServiceIOT :: m a
  }
  deriving newtype (Applicative, Functor, Monad)
  deriving (MonadTrans, MonadTransControl) via IdentityT

runTrackServiceIO :: TrackServiceIOT m a -> m a
runTrackServiceIO = runTrackServiceIOT

instance
  ( TrackRepository m,
    Logging m
  ) =>
  TrackService (TrackServiceIOT m)
  where
  importTracks mss = TrackServiceIOT $ do
    let ts = fmap newTracks mss
    tracks <- insert ts <&> fmap (^. #id) >>= getByKey
    pure (fmap from tracks)

newTracks :: Source -> NewTrack
newTracks ms = error "unimplemented"

--newTracks :: [(TrackSourceRef, MetadataId, Text)] -> [NewTrackSource] -> [NewTrack]
--newTracks ks tss = let srcMetadata = H.fromList $ fmap (\(k, mid, s) -> (s, (mid, k))) ks in
--  mapMaybe (metadataTracks srcMetadata) tss
--
--metadataTracks :: H.HashMap Text (MetadataId, TrackSourceRef) -> NewTrackSource -> Maybe NewTrack
--metadataTracks srcMetadata ts = do
--  let src = getSource ts
--  (metadataId, tsk) <- H.lookup src srcMetadata
--  m <- getMetadata metadataId ts
--  metadataTrack m tsk _ak (getInfo ts) (T.unpack src)
--
--fileTracks :: (Monad m) => MetadataFile -> TrackSourceRef -> m [NewTrack]
--fileTracks f k = case chooseMetadata (H.elems $ f ^. #metadata) of
--  Nothing -> pure []
--  Just metadata -> pure $ catMaybes [metadataTrack metadata k (f ^. #audioInfo) (f ^. #filePath)]

metadataTrack :: Metadata -> SourceRef -> AlbumRef -> Info -> FilePath -> NewTrack
metadataTrack Metadata {tags, lens} sk ak i p = do
  let trackTitle = fromMaybe "" (tags ^? lens M.trackTitle . _head)
  let trackNumber = case tags ^? lens M.trackNumber . _head of
        Just tnum -> parseTrackNumber tnum
        Nothing -> parseTrackNumberFromFileName p
  let trackComment = tags ^? lens M.commentTag . _head
  let discNumber = case tags ^? lens M.discNumberTag . _head of
        Just dnum -> parseDiscNumber dnum
        _noDiscNum -> case tags ^? lens M.trackNumber . _head of
          Just tnum -> parseDiscNumberFromTrackNumber tnum
          _noTrackNum -> Nothing
  NewTrack
    { title = trackTitle,
      trackNumber = trackNumber,
      discNumber = discNumber,
      comment = trackComment,
      length = audioLength i,
      sourceId = sk,
      albumId = ak
    }

--fileArtists ::
--  (Monad m, Has ArtistService sig m) =>
--  MetadataFile ->
--  m [NewArtist]
--fileArtists f = case chooseMetadata (H.elems $ f ^. #metadata) of
--  Nothing -> pure []
--  Just m -> do
--    -- let t = m ^. #tags
--    -- let tag = lens m
--    artists <- identifyArtists m
--    --    let x = artists ^.. #country
--    -- _newArtists
--    error "unimplemented"

--fileAlbums :: (Monad m) => MetadataFile -> m [NewAlbum]
--fileAlbums f = case chooseMetadata (H.elems $ f ^. #metadata) of
--  Nothing -> pure []
--  Just m -> do
--    let t = m ^. #tags
--    let tag = lens m
--    let albumTitle = t ^. tag M.album
--    --    _newAlbums
--    error "unimplemented"

data TrackNumber = TrackNumber
  { discNumber :: Maybe Int16,
    trackNumber :: Int16,
    totalTracks :: Maybe Int16
  }
  deriving (Generic)

trackNumParser :: Parser TrackNumber
trackNumParser = do
  skipSpace
  discNumber <- option Nothing (Just <$> decimal <* char '.')
  trackNumber <- skipSpace *> decimal <* skipSpace
  totalTracks <- option Nothing (Just <$> (char '/' >> skipSpace *> decimal))
  pure
    TrackNumber
      { discNumber,
        trackNumber,
        totalTracks
      }

--- parses track number tags of form "1", "01", "01/11", "1.01/11"
parseTrackNumber :: Text -> Maybe Int16
parseTrackNumber num = case parseOnly trackNumParser num of
  Right tn -> Just $ tn ^. #trackNumber
  Left _ -> Nothing

parseTrackNumberFromFileName :: FilePath -> Maybe Int16
parseTrackNumberFromFileName = parseTrackNumber . T.pack

parseDiscNumber :: Text -> Maybe Int16
parseDiscNumber num = case parseOnly decimal num of
  Right d -> Just d
  Left _ -> Nothing

parseDiscNumberFromTrackNumber :: Text -> Maybe Int16
parseDiscNumberFromTrackNumber num = case parseOnly trackNumParser num of
  Right d -> d ^. #discNumber
  Left _ -> Nothing
