{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Metadata.Service where

import Control.Algebra
import Control.Applicative
import Control.Carrier.Reader
import Control.Monad.IO.Class
import Data.Foldable
import Data.Functor
import Data.Text (Text)
import qualified Data.Text as T
import Melo.Common.Effect
import Melo.Format.Ape
import Melo.Format.ID3.ID3v1
import Melo.Format.ID3.ID3v2
import Melo.Format.Internal.Metadata
import Melo.Format.Vorbis
import Melo.Library.Metadata.Repo

data MetadataService :: Effect where
  ChooseMetadata :: [Metadata] -> MetadataService m (Maybe Metadata)

chooseMetadata :: Has MetadataService sig m => [Metadata] -> m (Maybe Metadata)
chooseMetadata ms = send (ChooseMetadata ms)

newtype MetadataServiceC m a
  = MetadataServiceC
      { runMetadataServiceC :: m a
      }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance
  (MonadIO m, Algebra sig m) =>
  Algebra (MetadataService :+: sig) (MetadataServiceC m)
  where
  alg hdl sig ctx = case sig of
    L (ChooseMetadata ms) -> do
      let m =
            find (\Metadata {..} -> formatId == vorbisCommentsId) ms
              <|> find (\Metadata {..} -> formatId == apeId) ms
              <|> find (\Metadata {..} -> formatId == id3v2Id) ms
              <|> find (\Metadata {..} -> formatId == id3v1Id) ms
      (ctx $>) <$> pure m
    R other -> MetadataServiceC (alg (runMetadataServiceC . hdl) other ctx)

runMetadataService :: MetadataServiceC m a -> m a
runMetadataService = runMetadataServiceC
