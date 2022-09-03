module Melo.Library.Source.Cue where

import Control.Applicative
import Control.Concurrent.Classy
import Control.Concurrent.Classy.Async
import Control.Exception.Safe
import Control.Lens hiding (from)
import Control.Monad.IO.Class
import Data.ByteString qualified as B
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe
import Data.Range
import Data.Text qualified as T
import Data.Time.LocalTime
import Data.Vector (Vector, fromList)
import Data.Vector qualified as V
import GHC.Natural (naturalToInteger)
import Melo.Common.Metadata
import Melo.Format.Mapping qualified as Mapping
import Melo.Format.Metadata
  ( Metadata (..),
    MetadataId (..),
    Tags (..),
    mappedTag,
  )
import Melo.Library.Source.Types
import System.FilePath
import Text.CueSheet

cueId :: MetadataId
cueId = MetadataId "CUE"

openCueFile ::
  ( MonadConc m,
    MonadIO m
  ) =>
  FilePath ->
  m (Vector CueFileSource)
openCueFile cueFilePath = do
  fileContents <- liftIO $ B.readFile cueFilePath
  case parseCueSheet (takeBaseName cueFilePath) fileContents of
    Left e -> throwIO e
    Right CueSheet {..} -> do
      let trackNums = [cueFirstTrackNumber ..]
      let cueTracks = cueFiles >>= \CueFile {..} -> cueFileTracks <&> (cueFileName,)
      let cueTracks' = zip trackNums (NE.toList cueTracks)
      let cueTracks'' = filter (\(_, (_, CueTrack {..})) -> cueTrackType == CueTrackAudio) cueTracks'
      processedTracks <- forConcurrently cueTracks'' $ \(trackNum, (filePath, CueTrack {..})) ->
        runMetadataServiceIO (openMetadataFile (takeDirectory cueFilePath </> filePath)) >>= \case
          Left e -> throwIO e
          Right mf -> do
            let rem = M.fromList $ bimap unCueText unCueText <$> cueRem
            let tags =
                  catMaybes
                    [ Just ("TRACKNUMBER", T.pack $ show trackNum),
                      Just ("TOTAL_TRACKS", T.pack $ show $ length cueTracks''),
                      ("PERFORMER",) . unCueText <$> (cueTrackPerformer <|> cuePerformer),
                      ("TITLE",) . unCueText <$> cueTrackTitle,
                      ("ALBUM_PERFORMER",) . unCueText <$> (cuePerformer <|> cueTrackPerformer),
                      ("ALBUM_TITLE",) . unCueText <$> cueTitle,
                      ("DATE",) <$> M.lookup "DATE" rem,
                      ("GENRE",) <$> M.lookup "GENRE" rem,
                      ("REPLAYGAIN_ALBUM_GAIN",) <$> M.lookup "REPLAYGAIN_ALBUM_GAIN" rem,
                      ("REPLAYGAIN_ALBUM_PEAK",) <$> M.lookup "REPLAYGAIN_ALBUM_PEAK" rem,
                      ("REPLAYGAIN_TRACK_GAIN",) <$> M.lookup "REPLAYGAIN_TRACK_GAIN" rem,
                      ("REPLAYGAIN_TRACK_PEAK",) <$> M.lookup "REPLAYGAIN_TRACK_PEAK" rem
                    ]
            let metadata =
                  Metadata
                    { tags = Tags (V.fromList tags),
                      formatId = cueId,
                      formatDesc = "CUE file",
                      lens = mappedTag Mapping.cue
                    }
            pure
              CueFileSource
                { metadata,
                  idx = fromIntegral trackNum,
                  audioInfo = mf ^. #audioInfo,
                  range = cueTimeRange (NE.head cueTrackIndices),
                  fileId = mf ^. #fileId,
                  filePath = mf ^. #filePath,
                  cueFilePath
                }
      pure $ fromList $ squashTimeRanges processedTracks
      where
        cueTimeRange (CueTime frames) = TimeRange (LowerBoundRange (Bound (framesToTime frames) Inclusive))
        framesToTime frames = CalendarDiffTime 0 (fromInteger (naturalToInteger frames) / 75.0)
        squashTimeRanges :: [CueFileSource] -> [CueFileSource]
        squashTimeRanges [] = []
        squashTimeRanges [a] = [a]
        squashTimeRanges (a : b : rest) = combineTimeRanges a b : squashTimeRanges (b : rest)
        combineTimeRanges :: CueFileSource -> CueFileSource -> CueFileSource
        combineTimeRanges o@CueFileSource {range = a} CueFileSource {range = b} = o & #range .~ bp a b
        bp :: AudioRange -> AudioRange -> AudioRange
        bp (TimeRange (LowerBoundRange a)) (TimeRange (LowerBoundRange Bound {..})) =
          TimeRange $ SpanRange a (Bound boundValue Exclusive)
        bp a _ = a
