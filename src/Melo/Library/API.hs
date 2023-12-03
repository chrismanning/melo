{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.API where

import Control.Concurrent.Classy
import Control.Monad.Reader
import Data.Morpheus.Types
import Data.Typeable
import GHC.OverloadedLabels ()
import Melo.Common.Config
import Melo.Common.FileSystem
import Melo.Common.Tracing
import Melo.Common.Uuid
import Melo.Library.Release.Repo
import Melo.Library.Release.ArtistName.Repo
import Melo.Library.Artist.Name.Repo
import Melo.Library.Collection.API
import Melo.Library.Collection.Aggregate
import Melo.Library.Source.API
import Melo.Library.Source.Transform qualified as Tr
import Melo.Library.Track.ArtistName.Repo
import Melo.Library.Track.Repo
import Melo.Lookup.Covers

data LibraryQuery m = LibraryQuery
  { sources :: SourcesArgs -> m (Vector (Source m)),
    sourceGroups :: SourceGroupsArgs -> m (Vector (SourceGroup m)),
    collections :: CollectionsArgs -> m (Vector (Collection m))
  }
  deriving (Generic)

instance Typeable m => GQLType (LibraryQuery m)

resolveLibrary ::
  ( Tr.MonadSourceTransform m,
    ReleaseRepository m,
    ReleaseArtistNameRepository m,
    ArtistNameRepository m,
    TrackArtistNameRepository m,
    TrackRepository m,
    MonadConc m,
    CoverService m,
    ConfigService m,
    Tracing m,
    Typeable m,
    UuidGenerator m
  ) =>
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
  { transformSources :: TransformSources m,
    collection :: m (CollectionMutation m)
  }
  deriving (Generic)

instance Typeable m => GQLType (LibraryMutation m)

libraryMutation ::
  ( MonadIO m,
    CollectionAggregate m,
    Tr.MonadSourceTransform m,
    ReleaseRepository m,
    ReleaseArtistNameRepository m,
    ArtistNameRepository m,
    TrackArtistNameRepository m,
    TrackRepository m,
    MonadConc m,
    FileSystem m,
    CoverService m,
    ConfigService m,
    Tracing m,
    Typeable m,
    UuidGenerator m
  ) =>
  ResolverM e (m :: Type -> Type) LibraryMutation
libraryMutation =
  lift $
    pure
      LibraryMutation
        { transformSources = transformSourcesImpl,
          collection = collectionMutation
        }
