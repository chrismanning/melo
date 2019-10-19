module Melo.Format.Tag where

import Control.Exception
import Data.Text
import Debug.Trace
import Melo.Format.Detect
import Melo.Format.Format
import Melo.Format.Internal.Tag as Tag
import Melo.Format.Mapping
import Melo.Format.Metadata
import Polysemy
import System.IO

data TagEnv
  = TagEnv
      { fieldSel :: FieldMappingSelector,
        envtags :: Tags
      }

data TagRead (m :: * -> *) a where
  ReadTags :: TagRead m Tags
  ReadTag :: TagMapping -> TagRead m [Text]
  ReadField :: FieldMappings -> TagRead m [Text]

makeSem ''TagRead

data TagWrite r where
  WriteTags :: Tags -> TagWrite ()

runTagReadPure :: TagEnv -> Sem '[TagRead] a -> a
runTagReadPure env a = run $ runTagReadPureM env a

runTagReadPureM :: TagEnv -> Sem (TagRead ': effs) a -> Sem effs a
runTagReadPureM env = interpret $ interpretTagRead env
  where
    interpretTagRead :: TagEnv -> TagRead m a -> Sem effs a
    interpretTagRead me tr =
      let sel = fieldSel me
          ts = envtags me
       in case tr of
            ReadTags -> return ts
            ReadTag t -> return $ getMappedTag sel t ts
            ReadField f -> return $ getTagByField ts (sel f)

runTagReadFromPath :: Sem '[TagRead, Embed IO] a -> FilePath -> IO a
runTagReadFromPath a p = do
  env <- runM $ readMetaEnvFromPath p
  runM $ runTagReadPureM env a

readMetaEnvFromPath :: Member (Embed IO) effs => FilePath -> Sem effs TagEnv
readMetaEnvFromPath p = embed (detect p) >>= \case
  Nothing -> throw UnknownFormat
  Just (DetectedP d) -> do
    let fieldSel' = getFieldSel d
    let hReadMetadata' = getHReadMetadata d
    t <- embed $ withBinaryFile p ReadMode $ \h -> do
      !t <- Tag.tags <$> hReadMetadata' h
      return t
    let env = TagEnv {fieldSel = fieldSel', envtags = t}
    return env

hReadMetaEnv :: Member (Embed IO) effs => Handle -> Sem effs TagEnv
hReadMetaEnv h = embed (hDetect h) >>= \case
  Nothing -> throw UnknownFormat
  Just (DetectedP d) -> do
    let fieldSel' = getFieldSel d
    let hReadMetadata' = getHReadMetadata d
    embed $ hTell h >>= \p -> traceM $ "hReadMetaEnv h is at " ++ show p
    !t <- Tag.tags <$> embed (hReadMetadata' h)
    embed $ hTell h >>= \p -> traceM $ "hReadMetaEnv h is at " ++ show p
    --    embed $ hSeek h AbsoluteSeek 0
    let env = TagEnv {fieldSel = fieldSel', envtags = t}
    return env

runTagReadM ::
  forall r a.
  Member (Embed IO) r =>
  FilePath ->
  Sem (TagRead ': r) a ->
  Sem r a
runTagReadM p a = do
  env <- readMetaEnvFromPath p
  runTagReadPureM env a

hRunTagReadM ::
  forall r a.
  Member (Embed IO) r =>
  Handle ->
  Sem (TagRead ': r) a ->
  Sem r a
hRunTagReadM h a = do
  env <- hReadMetaEnv h
  runTagReadPureM env a
