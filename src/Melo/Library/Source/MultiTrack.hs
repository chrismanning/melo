{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Source.MultiTrack where

import Control.Concurrent.Classy
import Control.Exception.Safe
import Control.Monad.Parallel
import Control.Monad.Base
import Control.Monad.Trans.Identity
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Data.Conduit.Audio
import Data.Conduit.Audio.Sndfile
import Data.Int
import Data.Range
import Data.Time
import Melo.Library.Source.Types
import Sound.File.Sndfile qualified as Sndfile
import System.Directory qualified as Dir
import System.FilePath

class Monad m => MultiTrack m where
  extractTrackTo :: FilePath -> AudioRange -> FilePath -> m ()

data MultiTrackError =
    NotMultiTrack
  | DecoderError String
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
  extractTrackTo s r d = lift $ extractTrackTo s r d

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
      MonadParallel,
      MonadThrow
    )
  deriving (MonadTrans, MonadTransControl) via IdentityT

runMultiTrackIO :: MultiTrackIOT m a -> m a
runMultiTrackIO = runMultiTrackIOT

instance MonadIO m => MultiTrack (MultiTrackIOT m) where
  extractTrackTo srcPath range destPath = liftIO $ do
    Dir.createDirectoryIfMissing True (takeDirectory destPath)
    let start = getStartTime range
    src <- sourceSndFrom @_ @Int16 start srcPath
    info <- Sndfile.getFileInfo srcPath
    let src' = case getEndTime range of
          Just end -> takeStart end src
          Nothing -> src
    runResourceT $ sinkSnd destPath (Sndfile.format info) src'
    where
      getStartTime :: AudioRange -> Duration
      getStartTime (TimeRange (SpanRange lower _upper)) = Seconds $ toSeconds $ boundValue lower
      getStartTime (TimeRange (LowerBoundRange lower)) = Seconds $ toSeconds $ boundValue lower
      getStartTime (TimeRange _) = Seconds 0
      getStartTime (SampleRange _range) = Frames 0
      getEndTime :: AudioRange -> Maybe Duration
      getEndTime (TimeRange (SpanRange lower upper)) = Just $ Seconds $ toSeconds (boundValue upper) - toSeconds (boundValue lower)
      getEndTime (TimeRange (LowerBoundRange _)) = Nothing
      getEndTime (TimeRange _) = Nothing
      getEndTime (SampleRange _) = Nothing
      toSeconds = realToFrac . nominalDiffTimeToSeconds . ctTime
