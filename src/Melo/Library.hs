{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Melo.Library where

import Data.Kind
import Data.Text (Text)
import Polysemy

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

data LibraryManagement (m :: Type -> Type) a where
  LibraryAddPath :: LibraryPath -> LibraryManagement m Stats
  LibraryUpdatePath :: LibraryPath -> LibraryManagement m Stats
  LibraryUpdateAll :: LibraryManagement m Stats
  LibraryDeletePath :: LibraryPath -> LibraryManagement m Stats
  LibraryDeleteAll :: LibraryManagement m Stats
  LibraryPaths :: LibraryManagement m [LibraryPath]

makeSem ''LibraryManagement

data FileSource (m :: Type -> Type) a where
  GetEntries :: Text -> FileSource m [Text]
