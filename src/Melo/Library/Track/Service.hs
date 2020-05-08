{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Track.Service where

import Basement.From
import Control.Algebra
import Control.Effect.Lift
import Control.Effect.Reader
import Control.Lens hiding (from, lens)
import Control.Monad
import Data.Attoparsec.Text
import Data.Foldable
import Data.Functor
import qualified Data.HashMap.Strict as H
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Traversable
import qualified Data.Vector as V
import Database.Beam as B hiding (char, insert)
import Database.Beam.Backend.SQL.BeamExtensions as B
import Database.Beam.Postgres as Pg
import Database.Beam.Postgres.Full as Pg
import Melo.Common.Effect
import Melo.Common.Logging
import Melo.Common.Metadata
import Melo.Format.Info
import Melo.Format.Internal.Metadata
import qualified Melo.Format.Mapping as M
import Melo.Library.Album.Repo
import Melo.Library.Album.Service
import Melo.Library.Artist.Repo
import Melo.Library.Artist.Service
import qualified Melo.Library.Database.Model as DB
import Melo.Library.Database.Query
import Melo.Library.Source.Types
import Melo.Library.Track.Repo
import Network.URI
import System.Directory

data Track = Track
  {
  }

instance From DB.Track Track where
  from = undefined

data TrackService :: Effect where
  ImportTracks :: [Source] -> TrackService m [Track]

importTracks :: Has TrackService sig m => [Source] -> m [Track]
importTracks = send . ImportTracks

newtype TrackServiceIOC m a = TrackServiceIOC
  { runTrackServiceIOC :: m a
  }
  deriving newtype (Applicative, Functor, Monad)

runTrackServiceIO :: TrackServiceIOC m a -> m a
runTrackServiceIO = runTrackServiceIOC

instance
  ( Has TrackRepository sig m,
    Has ArtistService sig m,
    Has AlbumService sig m,
    Has Logging sig m
  ) =>
  Algebra (TrackService :+: sig) (TrackServiceIOC m)
  where
  alg _ (L (ImportTracks mss)) ctx = do
    let ts = fmap newTracks mss
    tracks <- insertTracks ts >>= getTracks
    pure (ctx $> fmap from tracks)
  alg hdl (R other) ctx = TrackServiceIOC (alg (runTrackServiceIOC . hdl) other ctx)

newTracks :: Source -> NewTrack
newTracks ms = undefined

--newTracks :: [(DB.TrackSourceKey, MetadataId, Text)] -> [NewTrackSource] -> [NewTrack]
--newTracks ks tss = let srcMetadata = H.fromList $ fmap (\(k, mid, s) -> (s, (mid, k))) ks in
--  mapMaybe (metadataTracks srcMetadata) tss
--
--metadataTracks :: H.HashMap Text (MetadataId, DB.TrackSourceKey) -> NewTrackSource -> Maybe NewTrack
--metadataTracks srcMetadata ts = do
--  let src = getSource ts
--  (metadataId, tsk) <- H.lookup src srcMetadata
--  m <- getMetadata metadataId ts
--  metadataTrack m tsk _ak (getInfo ts) (T.unpack src)
--
--fileTracks :: (Monad m) => MetadataFile -> DB.TrackSourceKey -> m [NewTrack]
--fileTracks f k = case chooseMetadata (H.elems $ f ^. #metadata) of
--  Nothing -> pure []
--  Just metadata -> pure $ catMaybes [metadataTrack metadata k (f ^. #audioInfo) (f ^. #filePath)]

metadataTrack :: Metadata -> DB.SourceKey -> DB.AlbumKey -> Info -> FilePath -> NewTrack
metadataTrack m sk ak i p = do
  let t = m ^. #tags
  let tag = lens m
  let trackTitle = case V.toList $ t ^. tag M.trackTitle of
        (title : _) -> title
        _ -> ""
  let trackNumber = case V.toList $ t ^. tag M.trackNumber of
        (tnum : _) -> parseTrackNumber tnum
        _ -> parseTrackNumberFromFileName p
  let trackComment = case V.toList $ t ^. tag M.commentTag of
        [comment] -> Just comment
        _ -> Nothing
  let discNumber = case V.toList $ t ^. tag M.discNumberTag of
        (dnum : _) -> parseDiscNumber dnum
        _ -> case V.toList $ t ^. tag M.trackNumber of
          (tnum : _) -> parseDiscNumberFromTrackNumber tnum
          _ -> Nothing
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
--    undefined

--fileAlbums :: (Monad m) => MetadataFile -> m [NewAlbum]
--fileAlbums f = case chooseMetadata (H.elems $ f ^. #metadata) of
--  Nothing -> pure []
--  Just m -> do
--    let t = m ^. #tags
--    let tag = lens m
--    let albumTitle = t ^. tag M.album
--    --    _newAlbums
--    undefined

data TrackNumber = TrackNumber
  { discNumber :: Maybe Int,
    trackNumber :: Int,
    totalTracks :: Maybe Int
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
parseTrackNumber :: Text -> Maybe Int
parseTrackNumber num = case parseOnly trackNumParser num of
  Right tn -> Just $ tn ^. #trackNumber
  Left _ -> Nothing

parseTrackNumberFromFileName :: FilePath -> Maybe Int
parseTrackNumberFromFileName = parseTrackNumber . T.pack

parseDiscNumber :: Text -> Maybe Int
parseDiscNumber num = case parseOnly decimal num of
  Right d -> Just d
  Left _ -> Nothing

parseDiscNumberFromTrackNumber :: Text -> Maybe Int
parseDiscNumberFromTrackNumber num = case parseOnly trackNumParser num of
  Right d -> d ^. #discNumber
  Left _ -> Nothing
