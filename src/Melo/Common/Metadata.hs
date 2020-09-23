{-# LANGUAGE UndecidableInstances #-}

module Melo.Common.Metadata where

import Control.Algebra
import Control.Applicative
import Control.Effect.Lift
import Control.Effect.TH
import Control.Lens ((^.))
import Data.Coerce
import Data.Foldable
import qualified Data.Text as T
import Melo.Common.Effect
import Melo.Common.Logging
import Melo.Format (Metadata (..))
import qualified Melo.Format as F

chooseMetadata :: [Metadata] -> Maybe Metadata
chooseMetadata ms =
  find (\Metadata {..} -> formatId == F.vorbisCommentsId) ms
    <|> find (\Metadata {..} -> formatId == F.apeV2Id) ms
    <|> find (\Metadata {..} -> formatId == F.apeV1Id) ms
    <|> find (\Metadata {..} -> formatId == F.id3v24Id) ms
    <|> find (\Metadata {..} -> formatId == F.id3v23Id) ms
    <|> find (\Metadata {..} -> formatId == F.id3v1Id) ms

data MetadataService :: Effect where
  OpenMetadataFile :: FilePath -> MetadataService m F.MetadataFile
  ReadMetadataFile :: F.MetadataFileId -> FilePath -> MetadataService m F.MetadataFile
  WriteMetadataFile :: F.MetadataFile -> FilePath -> MetadataService m F.MetadataFile

makeSmartConstructors ''MetadataService

newtype MetadataServiceIOC m a = MetadataServiceIOC
  { runMetadataServiceIOC :: m a
  }
  deriving newtype (Functor, Applicative, Monad)

runMetadataServiceIO :: MetadataServiceIOC m a -> m a
runMetadataServiceIO = runMetadataServiceIOC

instance
  ( Has (Lift IO) sig m,
    Has Logging sig m
  ) =>
  Algebra (MetadataService :+: sig) (MetadataServiceIOC m)
  where
  alg _ (L sig) ctx =
    ctx $$!> sendIO case sig of
      OpenMetadataFile path -> F.openMetadataFile path
      ReadMetadataFile mfid path -> do
        F.MetadataFileFactory {readMetadataFile} <- getFactory mfid
        readMetadataFile path
      WriteMetadataFile mf path -> do
        F.MetadataFileFactory {writeMetadataFile, readMetadataFile} <- getFactory (mf ^. #fileId)
        writeMetadataFile mf path
        readMetadataFile path
    where
      getFactory :: F.MetadataFileId -> IO (F.MetadataFileFactory IO)
      getFactory mfid = case F.metadataFileFactoryIO mfid of
        Just fact -> pure fact
        Nothing -> do
          runStdoutLogging $ $(logError) $ T.pack "unknown metadata file id '" <> coerce mfid <> "'"
          undefined
  alg hdl (R other) ctx = MetadataServiceIOC (alg (runMetadataServiceIOC . hdl) other ctx)