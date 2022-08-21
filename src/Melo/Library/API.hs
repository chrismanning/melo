{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.API where

import Control.Concurrent.Classy
import Control.Lens hiding (from, (.=))
import Control.Monad
import Control.Monad.Reader
import Data.Kind
import Data.Morpheus.Kind
import Data.Morpheus.Types
import Data.Text as T hiding (null)
import Data.Typeable
import Data.Vector (Vector)
import GHC.Generics hiding (from)
import GHC.OverloadedLabels ()
import Melo.Common.FileSystem
import Melo.Common.Logging
import Melo.Common.Metadata
import Melo.Library.Collection.API
import Melo.Library.Collection.Repo
import Melo.Library.Collection.Service
import Melo.Library.Source.API
import Melo.Library.Source.Repo
import qualified Melo.Library.Source.Transform as Tr
import Melo.Lookup.MusicBrainz
import Melo.Metadata.Mapping.Repo
import Network.URI

data LibraryQuery m = LibraryQuery
  { sources :: SourcesArgs -> m (Vector (Source m)),
    sourceGroups :: m (Vector (SourceGroup m)),
    collections :: CollectionsArgs -> m (Vector (Collection m))
  }
  deriving (Generic)

instance Typeable m => GQLType (LibraryQuery m)

resolveLibrary ::
  Tr.MonadSourceTransform m =>
  ResolverQ e m LibraryQuery
resolveLibrary =
  lift $
    pure
      LibraryQuery
        { sources = resolveSources,
          sourceGroups = resolveSourceGroups,
          collections = resolveCollections
        }

data LibraryMutation (m :: Type -> Type) = LibraryMutation
  { stageSources :: StageSourcesArgs -> m (StagedSources m),
    transformSources :: TransformSources m,
    updateSources :: UpdateSourcesArgs -> m (UpdatedSources m),
    collection :: m (CollectionMutation m)
  }
  deriving (Generic)

instance Typeable m => GQLType (LibraryMutation m)

libraryMutation ::
  ( MonadIO m,
    CollectionService m,
    Tr.MonadSourceTransform m,
    FileSystem m
  ) =>
  ResolverM e (m :: Type -> Type) LibraryMutation
libraryMutation =
  lift $
    pure
      LibraryMutation
        { stageSources = stageSourcesImpl,
          transformSources = transformSourcesImpl,
          updateSources = updateSourcesImpl,
          collection = collectionMutation
        }

newtype StageSourcesArgs = StageSourcesArgs
  { uris :: [Text]
  }
  deriving (Generic)

instance GQLType StageSourcesArgs where
  type KIND StageSourcesArgs = INPUT

data StagedSources m = StagedSources
  { sources :: m [Source m],
    groups :: m [SourceGroup m],
    numberOfSourcesImported :: Int
  }
  deriving (Generic)

instance Typeable m => GQLType (StagedSources m)

instance Monad m => Semigroup (StagedSources m) where
  a <> b =
    StagedSources
      { sources = liftM2 (<>) (a ^. #sources) (b ^. #sources),
        groups = liftM2 (<>) (a ^. #groups) (b ^. #groups),
        numberOfSourcesImported = a ^. #numberOfSourcesImported + b ^. #numberOfSourcesImported
      }

instance (Semigroup (StagedSources m), Applicative m) => Monoid (StagedSources m) where
  mempty =
    StagedSources
      { sources = pure [],
        groups = pure [],
        numberOfSourcesImported = 0
      }

stageSourcesImpl ::
  ( SourceRepository m
  ) =>
  StageSourcesArgs ->
  ResolverM e m StagedSources
stageSourcesImpl (StageSourcesArgs ss) = lift $ do
  x <- forM ss $ \s ->
    case parseURI (T.unpack s) of
      Just srcUri -> case uriScheme srcUri of
        --        "file:" -> do
        --          srcs <- importPath (unEscapeString $ uriPath srcUri)
        --          pure
        --            StagedSources
        --              { numberOfSourcesImported = Prelude.length srcs,
        --                sources = pure (fmap from srcs),
        --                groups = pure []
        --              }
        _ -> pure mempty
      Nothing -> pure mempty
  pure $ mconcat x
