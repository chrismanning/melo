{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Filesystem where

import Control.Algebra
import Control.Carrier.Empty.Church
import Control.Carrier.Error.Church
import Control.Carrier.Lift
import Control.Exception.Safe
import Data.Pool
import Database.Beam.Postgres as Pg
import Melo.Common.FileSystem
import Melo.Common.Logging
import Melo.Common.Metadata
import Melo.Database.Transaction as Tr
import Melo.Format.Error
import Melo.Library.Album.Repo
import Melo.Library.Album.Service
import Melo.Library.Artist.Repo
import Melo.Library.Artist.Staging.Repo
import Melo.Library.Genre.Repo
import Melo.Library.Service
import Melo.Library.Source.Repo
import Melo.Library.Track.Repo
import Melo.Library.Track.Service
import Melo.Lookup.MusicBrainz
import Network.Wreq.Session (Session)
import qualified Network.Wreq.Session as Sess

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
    Has (Error MetadataException) sig m,
    Has MetadataService sig m,
    Has AlbumRepository sig m,
    Has AlbumService sig m,
    Has ArtistRepository sig m,
    Has ArtistStagingRepository sig m,
    Has GenreRepository sig m,
    Has TrackRepository sig m,
    Has TrackService sig m,
    Has FileSystem sig m,
    Has Logging sig m,
    Has Empty sig m
  )

runImporter sess =
  runEmpty (sendIO $ putStrLn "failed") (sendIO . print)
    . runStdoutLogging
    . runError
      ( \(e :: MetadataException) -> do
          $(logError) $ "Uncaught error: " <> show e
          undefined
      )
      pure
    . runFileSystemIO
    . runMusicBrainzServiceIO sess
    . runMetadataServiceIO
    . runAlbumRepositoryIO
    . runAlbumServiceIO
    . runArtistRepositoryIO
    . runArtistStagingRepositoryIO
    . runTrackRepositoryIO
    . runTrackServiceIO
    . runSourceRepositoryIO
    . runGenreRepositoryIO
    . runLibraryServiceIO

importPath' :: Pool Connection -> Session -> FilePath -> IO ()
importPath' pool sess dir = do
  runStdoutLogging $ Tr.runTransaction pool $
    Tr.withTransaction $
      runImporter sess $
        importPath dir
  pure ()
