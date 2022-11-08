{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Source.Aggregate where

import Control.Applicative hiding (empty)
import Control.Exception.Safe
import Control.Foldl (PrimMonad)
import Control.Lens (firstOf)
import Control.Monad.Base
import Control.Monad.Conc.Class
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.Identity
import Data.Char
import Data.Foldable
import Data.Text qualified as T
import Data.Vector (Vector, empty, singleton)
import Data.Vector qualified as V
import Melo.Common.FileSystem
import Melo.Common.Logging
import Melo.Common.Uri
import Melo.Common.Vector
import Melo.Database.Repo as Repo
import Melo.Library.Album.Aggregate
import Melo.Library.Source.Repo
import Melo.Library.Source.Types
import System.FilePath as P
import Witch

class Monad m => SourceAggregate m where
  importSources :: Vector NewImportSource -> m (Vector Source)

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTrans t,
    SourceAggregate m
  ) =>
  SourceAggregate (t m)
  where
  importSources = lift . importSources

newtype SourceAggregateIOT m a = SourceAggregateIOT
  { runSourceAggregateIOT :: m a
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
      MonadThrow,
      PrimMonad
    )
  deriving (MonadTrans, MonadTransControl) via IdentityT

instance
  ( SourceRepository m,
    AlbumAggregate m,
    Logging m
  ) =>
  SourceAggregate (SourceAggregateIOT m)
  where
  importSources ss | null ss = pure empty
  importSources ss = do
    $(logDebug) $ "Importing " <> show (V.length ss) <> " sources"
    let metadataSources = rights $ fmap tryFrom ss
    $(logDebug) $ "Importing " <> show (V.length metadataSources) <> " metadata sources"
    srcs <- rights . fmap tryFrom <$> insert (fmap (from @MetadataImportSource) metadataSources)
    -- TODO publish sources imported event
    _albums <- importAlbums srcs
    pure srcs

getAllSources :: SourceRepository m => m (Vector Source)
getAllSources = rights <$> fmap tryFrom <$> Repo.getAll

getSource :: SourceRepository m => SourceRef -> m (Maybe Source)
getSource key = do
  srcs <- Repo.getByKey (singleton key)
  pure $ firstOf traverse $ rights $ tryFrom <$> srcs

getSourcesByUriPrefix ::
  SourceRepository m =>
  URI ->
  m (Vector Source)
getSourcesByUriPrefix prefix = do
  srcs <- getByUriPrefix prefix
  pure (rights $ fmap tryFrom srcs)

length' :: (Foldable f, Num a) => f b -> a
length' = foldl' (const . (+ 1)) 0

getSourceFilePath :: (SourceRepository m) => SourceRef -> m (Maybe FilePath)
getSourceFilePath key = do
  s <- Repo.getSingle key
  pure (s >>= parseURI . T.unpack . (.source_uri) >>= uriToFilePath)

findCoverImage :: FileSystem m => FilePath -> m (Maybe FilePath)
findCoverImage p = do
  isDir <- doesDirectoryExist p
  if isDir
    then do
      entries <- listDirectory p
      pure $
        find (\e -> P.takeBaseName e =~= "cover" && isImage e) entries
          <|> find (\e -> P.takeBaseName e =~= "front" && isImage e) entries
          <|> find (\e -> P.takeBaseName e =~= "folder" && isImage e) entries
    else pure Nothing
  where
    a =~= b = fmap toLower a == fmap toLower b
    isImage :: FilePath -> Bool
    isImage p =
      let ext = toLower <$> takeExtension p
       in ext == ".jpeg"
            || ext == ".jpg"
            || ext == ".png"
