module Melo.Library.Filesystem where

import Conduit
import Control.Algebra
import Control.Applicative
import Control.Carrier.Empty.Church
import Control.Carrier.Reader
import Control.Effect.Reader
import Control.Effect.Sum
import Control.Exception.Safe
import Control.Lens ((^.))
import Control.Monad
import Control.Monad.Logger (LoggingT)
import Data.Attoparsec.Text
import Data.Foldable
import qualified Data.HashMap.Strict as H
import Data.List.NonEmpty as NE
import Data.Maybe
import Data.Pool
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.LocalTime
import Data.Vector (Vector)
import Database.Beam as B hiding (char, insert)
import Database.Beam.Backend.SQL.BeamExtensions as B
import Database.Beam.Postgres as Pg
import Database.Beam.Postgres.Full as Pg
import Database.PostgreSQL.Simple
import Debug.Trace
import GHC.Records
import Haxl.Core
import Melo.Common.Http
import Melo.Common.Logging
import Melo.Common.RateLimit
import Melo.Format.Ape
import Melo.Format.ID3.ID3v1
import Melo.Format.ID3.ID3v2
import Melo.Format.Info
import Melo.Format.Internal.Metadata
import qualified Melo.Format.Mapping as M
import Melo.Format.Metadata
import Melo.Format.Vorbis
import Melo.Library.Album.Repo
import Melo.Library.Artist.Repo
import Melo.Library.Artist.Service
import Melo.Library.Database.Model as BM
import Melo.Library.Genre.Repo
import Melo.Library.Metadata.Repo
import Melo.Library.Metadata.Service
import Melo.Library.Track.Repo
import Melo.Lookup.MusicBrainz
import Network.URI
import qualified Network.Wreq.Session as Sess
import Network.Wreq.Session (Session)
import System.Directory
import System.FilePath.Posix

importDeep :: Pool Connection -> FilePath -> IO ()
importDeep pool root = do
  isDir <- doesDirectoryExist root
  isFile <- doesFileExist root
  sess <- Sess.newSession
  if isDir
    then withResource pool \conn -> importDirectory conn sess root
    else
      when isFile $
        catchAny
          ( withResource pool $ \conn -> do
              f <- openMetadataFile root
              runImporter conn sess $ importMetadataFile f
          )
          print

type Importer sig m =
  ( Monad m,
    Has MusicBrainzService sig m,
    Has MetadataSourceRepository sig m,
    Has AlbumRepository sig m,
    Has ArtistRepository sig m,
    Has ArtistService sig m,
    Has MetadataService sig m,
    Has GenreRepository sig m,
    Has TrackRepository sig m,
    Has Logging sig m,
    Has Empty sig m
  )

runImporter conn sess =
  evalEmpty
    . runStdoutLogging
    . runMusicBrainzServiceIO sess
    . runMetadataSourceRepositoryIO conn
    . runAlbumRepositoryIO conn
    . runArtistRepositoryIO conn
    . runGenreRepositoryIO conn
    . runTrackRepositoryIO conn
    . runMetadataService
    . runArtistServiceIO

importDirectory :: Connection -> Session -> FilePath -> IO ()
importDirectory conn sess dir = do
  fs <- filterM doesFileExist =<< listDirectory dir
  metadataFiles <- mapM openMetadataFile fs
  withTransaction conn $ runImporter conn sess $
    mapM_ importMetadataFile metadataFiles

importMetadataFile :: Importer sig m => MetadataFile -> m ()
importMetadataFile f = do
  ks <- insertMetadataSources [FileMetadataSource f]
  insertTracks =<< newTracks f
  insertArtists =<< newArtists f
  insertAlbums =<< newAlbums f
  pure ()

newTracks :: (Monad m, Has MetadataService sig m) => MetadataFile -> m [NewTrack]
newTracks f = chooseMetadata (H.elems $ f ^. #metadata) >>= \case
  Nothing -> pure []
  Just metadata -> pure $ catMaybes [metadataTrack metadata (f ^. #audioInfo) (f ^. #filePath)]

newArtists ::
  (Monad m, Has ArtistService sig m, Has MetadataService sig m) =>
  MetadataFile ->
  m [NewArtist]
newArtists f = chooseMetadata (H.elems $ f ^. #metadata) >>= \case
  Nothing -> pure []
  Just m -> do
    -- let t = m ^. #tags
    -- let tag = lens m
    albumArtists <- identifyArtists m
    --    _newArtists
    undefined

newAlbums :: (Monad m, Has MetadataService sig m) => MetadataFile -> m [NewAlbum]
newAlbums f = chooseMetadata (H.elems $ f ^. #metadata) >>= \case
  Nothing -> pure []
  Just m -> do
    let t = m ^. #tags
    let tag = lens m
    let albumTitle = t ^. tag M.album
    --    _newAlbums
    undefined

metadataTrack :: Metadata -> Info -> FilePath -> Maybe NewTrack
metadataTrack m i p = do
  let t = m ^. #tags
  let tag = lens m
  let trackTitle = case t ^. tag M.trackTitle of
        (title : _) -> title
        _ -> ""
  let trackNumber = case t ^. tag M.trackNumber of
        (tnum : _) -> parseTrackNumber tnum
        _ -> parseTrackNumberFromFileName p
  let trackComment = case t ^. tag M.commentTag of
        [comment] -> Just comment
        _ -> Nothing
  let discNumber = case t ^. tag M.discNumberTag of
        (dnum : _) -> parseDiscNumber dnum
        _ -> case t ^. tag M.trackNumber of
          (tnum : _) -> parseDiscNumberFromTrackNumber tnum
          _ -> Nothing
  pure
    NewTrack
      { title = trackTitle,
        trackNumber = trackNumber,
        discNumber = discNumber,
        comment = trackComment,
        length = audioLength i
      }

data TrackNumber
  = TrackNumber
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
