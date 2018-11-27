module Melo.Tag where

import           Control.Exception
import           Control.Monad.Freer
import           Control.Monad.Freer.TH
import           Data.Text
import           System.IO

import           Melo.Detect
import           Melo.Format
import           Melo.Internal.Tag             as Tag
import           Melo.Mapping
import           Melo.Metadata

import           Debug.Trace

data TagEnv = TagEnv {
  fieldSel :: FieldMappingSelector
, envtags :: Tags
}

data TagRead r where
  ReadTags :: TagRead Tags
  ReadTag :: TagMapping -> TagRead [Text]
  ReadField :: FieldMappings -> TagRead [Text]

makeEffect ''TagRead

runTagReadPure :: TagEnv -> Eff '[TagRead] a -> a
runTagReadPure env a = run $ runTagReadPureM env a

runTagReadPureM :: TagEnv -> Eff (TagRead ': effs) ~> Eff effs
runTagReadPureM env = interpret $ interpretTagRead env
 where
  interpretTagRead :: TagEnv -> TagRead ~> Eff effs
  interpretTagRead me tr =
    let sel = fieldSel me
        ts  = envtags me
    in  case tr of
          ReadTags    -> return ts
          ReadTag   t -> return $ getMappedTag sel t ts
          ReadField f -> return $ getTagByField ts (sel f)

runTagReadFromPath :: Eff '[TagRead, IO] a -> FilePath -> IO a
runTagReadFromPath a p = do
  env <- runM $ readMetaEnvFromPath p
  runM $ runTagReadPureM env a

readMetaEnvFromPath :: Member IO effs => FilePath -> Eff effs TagEnv
readMetaEnvFromPath p = send (detect p) >>= \case
  Nothing            -> throw UnknownFormat
  Just (DetectedP d) -> do
    let fieldSel'      = getFieldSel d
    let hReadMetadata' = getHReadMetadata d
    t <- send $ withBinaryFile p ReadMode $ \h -> do
      !t <- Tag.tags <$> hReadMetadata' h
      return t
    let env = TagEnv {fieldSel = fieldSel', envtags = t}
    return env

hReadMetaEnv :: Member IO effs => Handle -> Eff effs TagEnv
hReadMetaEnv h = send (hDetect h) >>= \case
  Nothing            -> throw UnknownFormat
  Just (DetectedP d) -> do
    let fieldSel'      = getFieldSel d
    let hReadMetadata' = getHReadMetadata d
    send $ hTell h >>= \p -> traceM $ "hReadMetaEnv h is at " ++ show p
    !t <- Tag.tags <$> send (hReadMetadata' h)
    send $ hTell h >>= \p -> traceM $ "hReadMetaEnv h is at " ++ show p
--    send $ hSeek h AbsoluteSeek 0
    let env = TagEnv {fieldSel = fieldSel', envtags = t}
    return env

runTagReadM
  :: forall effs a
   . LastMember IO effs
  => FilePath
  -> Eff (TagRead ': effs) a
  -> Eff effs a
runTagReadM p a = do
  env <- readMetaEnvFromPath p
  runTagReadPureM env a

hRunTagReadM
  :: forall effs a
   . LastMember IO effs
  => Handle
  -> Eff (TagRead ': effs) a
  -> Eff effs a
hRunTagReadM h a = do
  env <- hReadMetaEnv h
  runTagReadPureM env a
