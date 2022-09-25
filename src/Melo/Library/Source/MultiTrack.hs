{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Source.MultiTrack where

import Control.Concurrent.Classy
import Control.Exception.Safe
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Resource
import Data.Conduit.Audio
import Data.Conduit.Audio.Sndfile
import Data.Either.Combinators
import Data.Int
import Data.Range
import Data.Time
import Melo.Common.Metadata
import Melo.Format.Error
import Melo.Format.Metadata (MetadataFile)
import Melo.Library.Source.Types
import Sound.File.Sndfile qualified as Sndfile
import System.Directory qualified as Dir
import System.FilePath

class Monad m => MultiTrack m where
  extractTrackTo :: CueFileSource -> FilePath -> m (Either MultiTrackError MetadataFile)

data MultiTrackError
  = NotMultiTrack
  | DecoderError String
  | MetadataError MetadataException
  deriving (Show)

instance Exception MultiTrackError

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTrans t,
    MultiTrack m
  ) =>
  MultiTrack (t m)
  where
  extractTrackTo s d = lift $ extractTrackTo s d

newtype MultiTrackIOT m a = MultiTrackIOT
  { runMultiTrackIOT :: m a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadBase b,
      MonadBaseControl b,
      MonadConc,
      MonadCatch,
      MonadMask,
      MonadThrow
    )
  deriving (MonadTrans, MonadTransControl) via IdentityT

runMultiTrackIO :: MultiTrackIOT m a -> m a
runMultiTrackIO = runMultiTrackIOT

instance (MetadataService m, MonadIO m) => MultiTrack (MultiTrackIOT m) where
  extractTrackTo cuefile destPath = do
    liftIO $ do
      Dir.createDirectoryIfMissing True (takeDirectory destPath)
      let start = getStartTime cuefile.range
      src <- sourceSndFrom @_ @Int16 start cuefile.filePath
      info <- Sndfile.getFileInfo cuefile.filePath
      let src' = case getEndTime cuefile.range of
            Just end -> takeStart end src
            Nothing -> src
      runResourceT $ sinkSnd destPath (Sndfile.format info) src'
    mapLeft MetadataError <$!> openMetadataFile destPath
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
