module Melo.Library.Filesystem where

import Basement.From
import Conduit
import Control.Algebra
import Control.Applicative
import Control.Bool
import Control.Carrier.Empty.Church
import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Effect.Reader
import Control.Effect.Sum
import Control.Exception.Safe
import Control.Lens hiding (lens)
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
import qualified Database.PostgreSQL.Simple as Pg
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
import Melo.Library.Album.Service
import Melo.Library.Artist.Repo
import Melo.Library.Artist.Staging.Repo
import Melo.Library.Artist.Service
import Melo.Database.Model as BM
import Melo.Database.Transaction as Tr
import Melo.Library.Genre.Repo
import Melo.Library.Service
import Melo.Library.Source.Repo
import Melo.Library.Source.Service
import Melo.Library.Track.Repo
import Melo.Library.Track.Service
import Melo.Lookup.MusicBrainz
import Network.URI
import qualified Network.Wreq.Session as Sess
import Network.Wreq.Session (Session)
import System.Directory
import System.FilePath.Posix

importDeep :: Pool Connection -> FilePath -> IO ()
importDeep pool root = do
  sess <- Sess.newSession
  putStrLn $ "Importing " <> root
  catchAny
    (importPath' pool sess root)
    print

type Importer sig m =
  ( Monad m,
    Has MusicBrainzService sig m,
    Has LibraryService sig m,
    Has AlbumRepository sig m,
    Has AlbumService sig m,
    Has ArtistRepository sig m,
    Has ArtistStagingRepository sig m,
    Has ArtistService sig m,
    Has GenreRepository sig m,
    Has TrackRepository sig m,
    Has TrackService sig m,
    Has Logging sig m,
    Has Empty sig m
  )

runImporter sess =
  runEmpty (sendIO $ putStrLn "failed") (sendIO . print)
    . runStdoutLogging
    . runMusicBrainzServiceIO sess
    . runAlbumRepositoryIO
    . runAlbumServiceIO
    . runArtistRepositoryIO
    . runArtistStagingRepositoryIO
    . runArtistServiceIO
    . runTrackRepositoryIO
    . runTrackServiceIO
    . runSourceRepositoryIO
    . runSourceServiceIO
    . runGenreRepositoryIO
    . runLibraryServiceIO

importPath' :: Pool Connection -> Session -> FilePath -> IO ()
importPath' pool sess dir = do
  Tr.runTransaction pool $ Tr.withTransaction $ runImporter sess $
    importPath dir
  pure ()
