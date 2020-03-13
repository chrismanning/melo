{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Metadata.Service where

import Control.Algebra
import Control.Applicative
import Control.Carrier.Reader
import Control.Monad.IO.Class
import Data.Foldable
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic, Generic1)
import Melo.Format.Ape
import Melo.Format.ID3.ID3v1
import Melo.Format.ID3.ID3v2
import Melo.Format.Internal.Metadata
import Melo.Format.Vorbis
import Melo.Library.Metadata.Repo

data MetadataService m k
  = ChooseMetadata [Metadata] (Maybe Metadata -> m k)
  deriving (Functor, Generic1, HFunctor, Effect)

chooseMetadata :: Has MetadataService sig m => [Metadata] -> m (Maybe Metadata)
chooseMetadata ms = send (ChooseMetadata ms pure)

newtype MetadataServiceC m a
  = MetadataServiceC
      { runMetadataServiceC :: m a
      }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance
  (MonadIO m, Algebra sig m, Effect sig) =>
  Algebra (MetadataService :+: sig) (MetadataServiceC m)
  where
  alg = \case
    L (ChooseMetadata ms k) -> do
      let m =
            find (\Metadata {..} -> formatId == vorbisCommentsId) ms
              <|> find (\Metadata {..} -> formatId == apeId) ms
              <|> find (\Metadata {..} -> formatId == id3v2Id) ms
              <|> find (\Metadata {..} -> formatId == id3v1Id) ms
      k m
    R other -> MetadataServiceC (alg (handleCoercible other))

runMetadataService :: MetadataServiceC m a -> m a
runMetadataService = runMetadataServiceC
