{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Melo.Library where

import Control.Monad
import Data.Text (Text)
import Melo.Format.Detect
import Polysemy
import System.Directory

data Stats
  = Stats
      { totalTracks :: Integer,
        tracksAdded :: Integer,
        tracksRemoved :: Integer,
        tracksUpdated :: Integer
      }

data LibraryRoot

data LibraryPath
  = LibraryDirectory FilePath
  | LibraryFile FilePath
  deriving (Eq, Ord, Show)

data LibraryManagement (m :: * -> *) a where
  LibraryAddPath :: LibraryPath -> LibraryManagement m Stats
  LibraryUpdatePath :: LibraryPath -> LibraryManagement m Stats
  LibraryUpdateAll :: LibraryManagement m Stats
  LibraryDeletePath :: LibraryPath -> LibraryManagement m Stats
  LibraryDeleteAll :: LibraryManagement m Stats
  LibraryPaths :: LibraryManagement m [LibraryPath]

makeSem ''LibraryManagement

data FileSource (m :: * -> *) a where
  GetEntries :: Text -> FileSource m [Text]

scanPath :: FilePath -> IO ()
scanPath root = do
  isFile <- doesFileExist root
  isDir <- doesDirectoryExist root
  when isFile $
    detect root >>= \case
      Just d -> importDetected d
      Nothing -> pure ()
  when isDir $
    mapM_ scanPath =<< listDirectory root

importDetected :: DetectedP -> IO ()
importDetected = do
  undefined
