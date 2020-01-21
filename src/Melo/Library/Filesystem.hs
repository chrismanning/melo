module Melo.Library.Filesystem where

import Conduit
import Control.Applicative
import Control.Exception.Safe
import Control.Lens ((^.))
import Control.Monad
import Data.Attoparsec.Text
import Data.Foldable
import qualified Data.HashMap.Strict as H
import Data.List.NonEmpty as NE
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
import Melo.Format.Ape
import Melo.Format.ID3.ID3v1
import Melo.Format.ID3.ID3v2
import Melo.Format.Info
import Melo.Format.Internal.Metadata
import qualified Melo.Format.Mapping as M
import Melo.Format.Metadata
import Melo.Format.Vorbis
import Melo.Library.Database.Model as BM
import Melo.Library.Repo.Metadata
import Network.URI
import System.Directory
import System.FilePath.Posix

scanPath :: Pool Connection -> FilePath -> IO ()
scanPath pool root = do
  isDir <- doesDirectoryExist root
  isFile <- doesFileExist root
  if isDir
    then do
      metadataFiles :: Vector MetadataFile <- runConduit $ sourceDirectory root
        .| iterMC print
        .| mapMC openMetadataFile
        .| sinkVector
      mapM_ (scanPath pool . (root </>)) =<< listDirectory root
    else
      when isFile $
        catchAny (openMetadataFile root >>= \f -> importMetadataFile pool f) print

-- TODO proper logging
-- TODO abstract DB
importMetadataFile :: Pool Connection -> MetadataFile -> IO ()
importMetadataFile pool f = do
  traceM $ "importing " <> f ^. #filePath
  let stateStore = stateSet MetadataSourceState {connPool = pool} stateEmpty
  env <- initEnv stateStore ()
  ks <- runHaxl env (insertMetadataSource $ FileMetadataSource f)
  case ks of
    [] -> pure ()
    (k : ks') -> importMetadata (k :| ks')
  where
    importMetadata :: NonEmpty MetadataSourceKey -> IO ()
    importMetadata ks = do
      let allMetadata = f ^. #metadata
      case chooseMetadataFormat $ fromList (H.elems allMetadata) of
        Nothing -> pure ()
        Just metadata -> do
          let tag = lens metadata
          let tags' = tags metadata
          let albumArtist = tags' ^. tag M.albumArtist
          let albumTitle = tags' ^. tag M.album
          let trackTitle = case tags' ^. tag M.trackTitle of
                [title] -> title
                _ -> ""
          let trackNumber = case tags' ^. tag M.trackNumber of
                [tnum] -> parseTrackNumber tnum
                _ -> parseTrackNumberFromFileName (f ^. #filePath)
          let trackComment = case tags' ^. tag M.commentTag of
                [comment] -> Just comment
                _ -> Nothing
          let discNumber = case tags' ^. tag M.discNumberTag of
                [dnum] -> parseDiscNumber dnum
                _ -> case tags' ^. tag M.trackNumber of
                  [tnum] -> parseDiscNumberFromTrackNumber tnum
                  _ -> Nothing
          let i = f ^. #audioInfo
          let (MetadataId metadataId) = getField @"formatId" metadata
          runBeamPostgresDebug traceM conn $ do
            let sources = filter_ (\src -> (src ^. #id `in_` fmap (\(MetadataSourceKey kid) -> val_ kid) (NE.toList ks)) &&. (src ^. #kind ==. val_ metadataId)) (all_ $ libraryDb ^. #metadata_source)
            runSelectReturningOne (select (fmap primaryKey sources)) >>= \case
              Nothing -> pure ()
              Just metadataSource -> do
                Pg.runPgInsertReturningList $
                  Pg.insertReturning
                    (libraryDb ^. #track)
                    ( insertExpressions
                        [ BM.Track
                            { id = default_,
                              title = val_ trackTitle,
                              track_number = val_ trackNumber,
                              comment = val_ trackComment,
                              album_id = nothing_,
                              disc_number = val_ discNumber,
                              audio_source_id = nothing_,
                              metadata_source_id = val_ metadataSource,
                              length = fromMaybe_ (val_ (Interval 0)) (val_ (Interval <$> audioLength i))
                            }
                        ]
                    )
                    Pg.onConflictDefault
                    (Just primaryKey)
                pure ()
          pure ()

chooseMetadataFormat :: NonEmpty Metadata -> Maybe Metadata
chooseMetadataFormat ms =
  find (\m -> getField @"formatId" m == vorbisCommentsId) ms
    <|> find (\m -> getField @"formatId" m == apeId) ms
    <|> find (\m -> getField @"formatId" m == id3v2Id) ms
    <|> find (\m -> getField @"formatId" m == id3v1Id) ms

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
  pure TrackNumber
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
