module Melo.Format.Format
  ( getTagByField,
    getMappedTag,
    MetadataFormat (..),
    hGetMetadata,
  )
where

import Control.Monad
import Control.Monad.Fail as F
import qualified Data.ByteString.Lazy as L
import Data.Foldable
  ( concat,
    find,
  )
import Data.Text (Text, unpack)
import Debug.Trace
import Melo.Format.Internal.Binary
import Melo.Format.Internal.BinaryUtil
import Melo.Format.Internal.Format
import Melo.Format.Internal.Locate
import Melo.Format.Internal.Tag
import Melo.Format.Mapping
import System.IO

getMappedTag :: FieldMappingSelector -> TagMapping -> Tags -> [Text]
getMappedTag _ (TagMapping []) _ = []
getMappedTag s (TagMapping ms) t =
  let ms' = fmap s ms
   in concat $ find (not . null) (fmap (getTagByField t) ms')

getTagByField :: Tags -> FieldMapping -> [Text]
getTagByField (Tags []) _ = []
getTagByField (Tags ts) m = fmap snd . filter (matches m . fst) $ ts
  where
    matches :: FieldMapping -> Text -> Bool
    matches NoFieldMapping _ = False
    matches fm v = fieldMatcher fm v

hGetMetadata :: forall a. MetadataLocator a => Handle -> IO a
hGetMetadata h = do
  bs <- hLocate @a h >>= \case
    Nothing -> F.fail $ "Unable to locate " ++ unpack (formatDesc @a)
    Just i -> do
      traceIO $ "Found " ++ unpack (formatDesc @a) ++ " at " ++ show i
      hIsClosed h >>= \c -> traceIO $ "h is " ++ if c then "closed" else "open"
      hSeek h AbsoluteSeek (fromIntegral i)
      hTell h >>= \p -> traceIO $ "h is at " ++ show p
      hIsClosed h >>= \c -> traceIO $ "h is " ++ if c then "closed" else "open"
      hGetFileContents h
  when (L.null bs) $ traceIO "no bytes"
  return $ bdecode bs
