{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.API where

import Basement.From
import Control.Algebra
import Control.Effect.Lift
import Control.Effect.Reader
import Control.Lens hiding (from, (.=))
import Control.Monad
import Data.Kind
import Data.Morpheus.Kind
import Data.Morpheus.Types
import Data.Text as T hiding (null)
import Database.Beam hiding (C)
import Database.Beam.Postgres (Connection)
import GHC.OverloadedLabels ()
import Melo.Common.FileSystem
import Melo.Common.Logging
import Melo.Common.Metadata
import Melo.Library.Collection.API
import Melo.Library.Collection.Repo
import Melo.Library.Source.API
import Melo.Library.Source.Repo
import Network.URI

data LibraryQuery m = LibraryQuery
  { sources :: SourcesArgs -> m [Source m],
    sourceGroups :: m [SourceGroup m],
    collections :: CollectionsArgs -> m [Collection m]
  }
  deriving (Generic)

instance Typeable m => GQLType (LibraryQuery m)

resolveLibrary ::
  forall sig m e.
  ( Has SourceRepository sig m,
    Has CollectionRepository sig m,
    Has FileSystem sig m
  ) =>
  ResolveQ e m LibraryQuery
resolveLibrary =
  lift $
    pure
      LibraryQuery
        { sources = resolveSources @sig @m,
          sourceGroups = resolveSourceGroups @sig,
          collections = resolveCollections @sig @m
        }

data LibraryMutation (m :: Type -> Type) = LibraryMutation
  { stageSources :: StageSourcesArgs -> m (StagedSources m),
    updateSources :: UpdateSourcesArgs -> m UpdatedSources
  }
  deriving (Generic)

instance Typeable m => GQLType (LibraryMutation m)

resolveLibraryMutation ::
  forall sig m e.
  ( Has SourceRepository sig m,
    Has (Lift IO) sig m,
    Has (Reader Connection) sig m,
    Has Logging sig m,
    Has MetadataService sig m
  ) =>
  ResolveM e (m :: Type -> Type) LibraryMutation
resolveLibraryMutation =
  lift $
    pure
      LibraryMutation
        { stageSources = stageSourcesImpl @sig @m,
          updateSources = updateSourcesImpl @sig @m
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
  ( Has SourceRepository sig m {-, Has LibraryService sig m-}
  ) =>
  StageSourcesArgs ->
  ResolveM e m StagedSources
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
