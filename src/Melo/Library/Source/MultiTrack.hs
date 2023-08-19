{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnboxedTuples #-}

module Melo.Library.Source.MultiTrack where

import Control.Monad.State.Strict
import Control.Monad.Trans.Resource
import Data.Conduit.Audio
import Data.Conduit.Audio.Sndfile
import Data.Int
import Data.Range
import Data.Time
import Melo.Common.Exception
import Melo.Common.FileSystem.Watcher
import Melo.Common.Monad
import Melo.Metadata.Aggregate
import Melo.Format.Error
import Melo.Format.Metadata (MetadataFile)
import Melo.Library.Source.Types
import Sound.File.Sndfile qualified as Sndfile
import System.Directory qualified as Dir
import System.FilePath

class Monad m => MultiTrack m where
  extractTrackTo :: CueFileSource -> FilePath -> m (Either MultiTrackError MetadataFile)

instance MultiTrack m => MultiTrack (StateT s m) where
  extractTrackTo s p = lift (extractTrackTo s p)

data MultiTrackError
  = NotMultiTrack
  | DecoderError String
  | MetadataError MetadataException
  deriving (Show)
  deriving TextShow via FromStringShow MultiTrackError

instance Exception MultiTrackError

instance MultiTrack (AppM IO IO) where
  extractTrackTo cuefile destPath = lockPathsDuring (takeDirectory destPath :| []) do
    liftIO do
      Dir.createDirectoryIfMissing True (takeDirectory destPath)
      let start = getStartTime cuefile.range
      src <- sourceSndFrom @_ @Int16 start cuefile.filePath
      info <- Sndfile.getFileInfo cuefile.filePath
      let src' = case getEndTime cuefile.range of
            Just end -> takeStart end src
            Nothing -> src
      runResourceT $ sinkSnd destPath (Sndfile.format info) src'
    first MetadataError <$!> openMetadataFile destPath
    where
      getStartTime :: AudioRange -> Duration
      getStartTime (TimeRange (SpanRange lower _upper)) = Seconds $ toSeconds $ boundValue lower
      getStartTime (TimeRange (LowerBoundRange lower)) = Seconds $ toSeconds $ boundValue lower
      getStartTime (TimeRange _) = Seconds 0
      getEndTime :: AudioRange -> Maybe Duration
      getEndTime (TimeRange (SpanRange lower upper)) = Just $ Seconds $ toSeconds (boundValue upper) - toSeconds (boundValue lower)
      getEndTime (TimeRange (LowerBoundRange _)) = Nothing
      getEndTime (TimeRange _) = Nothing
      toSeconds = realToFrac . nominalDiffTimeToSeconds . ctTime
