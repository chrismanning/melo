module Melo.Library.API where

import Control.Lens hiding ((.=))
import Control.Monad
import Data.Aeson as A
import Data.Aeson.Encoding as A
import Data.Generic.HKD
import Data.Text as T hiding (null)
import Data.Traversable
import Data.UUID (toText)
import Database.Beam hiding (C)
import GHC.Generics (Generic)
import GHC.OverloadedLabels ()
import Melo.API.Haxl
import Melo.GraphQL.Introspect
import Melo.GraphQL.Resolve
import qualified Melo.Library.Database.Model as BM
import Melo.Library.Repo.Genre

import Debug.Trace

data Library
  = Library
      { genres :: [Genre],
        artists :: [Artist],
        albums :: [Album],
        tracks :: [Track]
      }
  deriving (Generic)

instance GraphQLType Library where
  type TypeKind Library = 'ObjectKind

data LibraryCtx = LibraryCtx

instance ObjectResolver Haxl Library where
  type ResolverContext Library = LibraryCtx

instance GenericResolver Haxl Library where
  genericResolver = let g = build @Library in
    g (Resolve $ \_ _ fs -> do
        genres <- getAllGenres
        hydratedGenres <- for genres $ \genre ->
          resolveFieldValues genre fs
        traceShowM hydratedGenres
        pure $ A.list (\x -> x) hydratedGenres
      )
      (nullresolver)
      (nullresolver)
      (nullresolver)

data Genre
  = Genre
      { id :: Text,
        name :: Text,
        tracks :: [Track]
      }
  deriving (Generic)

instance GraphQLType Genre where
  type TypeKind Genre = 'ObjectKind

instance ObjectResolver Haxl Genre where
  type ResolverContext Genre = BM.Genre

instance GenericResolver Haxl Genre where
  genericResolver = let g = build @Genre in
    g (pureCtxResolver (toText . (^. #id)))
      (pureCtxResolver (^. #name))
      (Resolve $ \ctx _ fields -> do
        tracks <- getGenreTracks (primaryKey ctx)
        fmap (A.list (\x -> x)) $ forM tracks $ \track ->
          resolveFieldValues track fields
      )

data Artist
  = Artist
      { id :: Text,
        name :: Text,
        biography :: Maybe Text,
        shortBio :: Maybe Text,
        country :: Maybe Text,
        genres :: [Genre],
        albums :: [Album],
        tracks :: [Track]
      } deriving (Generic)

instance GraphQLType Artist where
  type TypeKind Artist = 'ObjectKind

instance ObjectResolver Haxl Artist where
  type ResolverContext Artist = BM.Artist

instance GenericResolver Haxl Artist where
  genericResolver = let a = build @Artist in
    a (pureCtxResolver (toText . (^. #id)))
      (pureCtxResolver (^. #name))
      (pureCtxResolver (^. #bio))
      (pureCtxResolver (^. #short_bio))
      (pureCtxResolver (^. #country))
      (nullresolver)
      (nullresolver)
      (nullresolver)

data Album
  = Album
      { id :: Text,
        title :: Text
      } deriving (Generic, ToJSON)

instance GraphQLType Album where
  type TypeKind Album = 'ObjectKind

instance ObjectResolver Haxl Album where
  type ResolverContext Album = BM.Album

instance GenericResolver Haxl Album where
  genericResolver = let a = build @Album in
    a (pureCtxResolver (toText . (^. #id)))
      (pureCtxResolver (^. #title))

data Track
  = Track
      { id :: Text,
        title :: Text
      }
  deriving (Generic, ToJSON)

instance GraphQLType Track where
  type TypeKind Track = 'ObjectKind

instance ObjectResolver Haxl Track where
  type ResolverContext Track = BM.Track

instance GenericResolver Haxl Track where
  genericResolver = let t = build @Track in
    t (pureCtxResolver (toText . (^. #id)))
      (pureCtxResolver (^. #title))
