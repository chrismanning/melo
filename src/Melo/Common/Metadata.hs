module Melo.Common.Metadata where

import Control.Applicative
import Data.Foldable
import Melo.Format

chooseMetadata :: [Metadata] -> Maybe Metadata
chooseMetadata ms =
  find (\Metadata {..} -> formatId == vorbisCommentsId) ms
    <|> find (\Metadata {..} -> formatId == apeV2Id) ms
    <|> find (\Metadata {..} -> formatId == apeV1Id) ms
    <|> find (\Metadata {..} -> formatId == id3v24Id) ms
    <|> find (\Metadata {..} -> formatId == id3v23Id) ms
    <|> find (\Metadata {..} -> formatId == id3v1Id) ms
