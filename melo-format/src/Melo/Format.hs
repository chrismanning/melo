module Melo.Format
  ( getTagByField
  , getMappedTag
  , MetadataFormat(..)
  , hGetMetadata
  )
where

import           Control.Monad
import           Control.Monad.Fail            as F
import qualified Data.ByteString.Lazy          as L
import           Data.Foldable                            ( concat
                                                          , find
                                                          )
import           Data.Text                                ( Text )
import           System.IO

import           Melo.Internal.Binary
import           Melo.Internal.BinaryUtil
import           Melo.Internal.Format
import           Melo.Internal.Locate
import           Melo.Internal.Tag
import           Melo.Mapping

import           Debug.Trace

getMappedTag :: FieldMappingSelector -> TagMapping -> Tags -> [Text]
getMappedTag _ (TagMapping []) _ = []
getMappedTag s (TagMapping ms) t =
  let ms' = fmap s ms
  in  concat $ find (not . null) (fmap (getTagByField t) ms')

getTagByField :: Tags -> FieldMapping -> [Text]
getTagByField (Tags []) _ = []
getTagByField (Tags ts) m = fmap snd . filter (matches m . fst) $ ts
 where
  matches :: FieldMapping -> Text -> Bool
  matches (FieldMapping _ f) v = f v
  matches _                  _ = False

hGetMetadata :: forall a . MetadataLocator a => Handle -> IO a
hGetMetadata h = do
  bs <- hLocate @a h >>= \case
    Nothing -> F.fail $ "Unable to locate " ++ formatDesc @a
    Just i  -> do
      traceIO $ "Found " ++ formatDesc @a ++ " at " ++ show i
      hIsClosed h >>= \c -> traceIO $ "h is " ++ if c then "closed" else "open"
      hSeek h AbsoluteSeek (fromIntegral i)
      hTell h >>= \p -> traceIO $ "h is at " ++ show p
      hIsClosed h >>= \c -> traceIO $ "h is " ++ if c then "closed" else "open"
      hGetFileContents h
  when (L.null bs) $ traceIO "no bytes"
  return $ bdecode bs
