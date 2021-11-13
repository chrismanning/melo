{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Source.Transform (
  Transform,
--  Transformation(..),
  MetadataTransformation(..),
  MonadSourceTransform,
  FileSystemPreviewT(..),
  applyTransformations,
  previewTransformations,
) where

import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Conc.Class
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Data.Bifunctor
import Data.List.NonEmpty
import Data.Text (Text)
import Melo.Common.FileSystem as FS
import Melo.Common.Logging
import Melo.Common.Uri
import Melo.Format
import qualified Melo.Format.Mapping as M
import Melo.Lookup.MusicBrainz
import Melo.Library.Source.Repo
import Melo.Library.Collection.Repo
import Melo.Library.Collection.Service
import Melo.Library.Collection.Types
import Melo.Library.Source.Types
import System.FilePath
import System.IO (TextEncoding)
import Witch
import Melo.Common.Metadata

type Transform m = [Source] -> m [Source]

data TransformAction where
  Move :: NonEmpty SourcePathPattern -> TransformAction
  ExtractEmbeddedImage :: URI -> TransformAction
  MoveCoverImage :: URI -> URI -> TransformAction
  SplitMultiTrackFile :: NonEmpty SourcePathPattern -> TransformAction
  RemoveOtherFiles :: TransformAction
  MusicBrainzLookup :: TransformAction
  ConvertEncoding :: TextEncoding -> TransformAction
  EditMetadata :: MetadataTransformation -> TransformAction

--evalTransformAction :: MonadSourceTransform m => TransformAction -> Transform m
--evalTransformAction (Move patterns) = mapM (moveSourceWithPattern patterns)

--data SourceGroup
--
--transformSourceGroup :: SourceGroup -> Transformation a -> m a
--transformSourceGroup = error "transformSourceGroup unimplemented"

type MonadSourceTransform m = (
    Monad m,
    FileSystem m,
    SourceRepository m,
    CollectionRepository m,
    MusicBrainzService m,
    MetadataService m,
    Logging m
  )

applyTransformations :: MonadSourceTransform m => [Transform m] -> [Source] -> m [Source]
applyTransformations transformations srcs = foldM transform srcs transformations
  where
    transform :: [Source] -> Transform m -> m [Source]
    transform srcs f = f srcs

previewTransformations :: MonadSourceTransform m => [Transform (FileSystemPreviewT m)] -> [Source] -> m [Source]
previewTransformations transformations srcs = runFileSystemPreviewT $ foldM previewTransform srcs transformations
  where
    previewTransform srcs f = f srcs

newtype FileSystemPreviewT m a = FileSystemPreviewT
  { runFileSystemPreviewT :: m a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadBase b, MonadBaseControl b, MonadConc, MonadCatch, MonadThrow, MonadMask)
  deriving (MonadTrans) via IdentityT

instance MonadSourceTransform m => FileSystem (FileSystemPreviewT m) where
  doesFileExist = lift . doesFileExist
  doesDirectoryExist = lift . doesDirectoryExist
  listDirectory = lift . listDirectory
  canonicalizePath = lift . canonicalizePath
  readFile = lift . FS.readFile
  movePath _ _ = lift $ do
    $(logDebug) ("Mocked movePath called" :: String)
    pure $ Right ()

--transformSource :: ( FileSystem m,
--                                  SourceRepository m,
--                                  CollectionRepository m,
--                                  MusicBrainzService m,
--                                  Logging m
--                                ) => Transformation a -> Source -> m (Either TransformationError a)
--transformSource (Move patterns) Source {ref} = first from <$> moveSourceWithPattern patterns ref
--transformSource (ExtractEmbeddedImage path) src = undefined
--transformSource (MoveCoverImage fromUri toUri) src = undefined
--transformSource (SplitMultiTrackFile patterns) src = undefined
--transformSource RemoveOtherFiles src = undefined
--transformSource MusicBrainzLookup src = undefined
--transformSource (ConvertEncoding encoding) src = undefined
--transformSource (EditMetadata edit) src = undefined

data TransformationError = MoveTransformError SourceMoveError
  deriving (Show)

instance From SourceMoveError TransformationError where
  from = MoveTransformError

data MetadataTransformation =
    SetMapping M.TagMapping [Text]
  | RemoveMapping M.TagMapping
  | Retain [M.TagMapping]
