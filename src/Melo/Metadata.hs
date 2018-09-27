module Melo.Metadata where

import Control.Exception
import Control.Monad.Freer
import Data.Text
import System.IO

import Melo.Detect
import Melo.Format(Tags, getMappedTag, getTagByField)
import qualified Melo.Format as Fmt (tags)
import Melo.Mapping

data MetaEnv = MetaEnv {
  fieldSel :: FieldMappingSelector
, tags :: Tags
, source :: Source
}

data Source = FSSource FilePath
  deriving (Show)

data MetaRead r where
  ReadTags :: MetaRead Tags
  ReadTag :: TagMapping -> MetaRead [Text]
  ReadField :: FieldMappings -> MetaRead [Text]

readTags :: Member MetaRead effs => Eff effs Tags
readTags = send ReadTags

readTag :: Member MetaRead effs => TagMapping -> Eff effs [Text]
readTag = send . ReadTag

readField :: Member MetaRead effs => FieldMappings -> Eff effs [Text]
readField = send . ReadField

runMetaReadPure :: MetaEnv -> Eff '[MetaRead] a -> a
runMetaReadPure env a = run $ runMetaReadPureM env a

runMetaReadPureM :: MetaEnv -> Eff (MetaRead ': effs) ~> Eff effs
runMetaReadPureM env = interpret $ interpretMetaRead env
  where
    interpretMetaRead :: MetaEnv -> MetaRead ~> Eff effs
    interpretMetaRead me mr = let sel = fieldSel me
                                  ts = tags me
                                 in
      case mr of
        ReadTags -> return ts
        ReadTag t -> return $ getMappedTag sel t ts
        ReadField f -> return $ getTagByField ts (sel f)

runMetaReadFromPath :: Eff '[MetaRead, IO] a -> FilePath -> IO a
runMetaReadFromPath a p = do
  env <- runM $ readMetaEnvFromPath p
  runM $ runMetaReadPureM env a

data MetadataException = UnknownFormat Source
  deriving Show

instance Exception MetadataException

readMetaEnvFromPath :: Member IO effs => FilePath -> Eff effs MetaEnv
readMetaEnvFromPath p = send (detect p) >>= \case
    Nothing -> throw $ UnknownFormat (FSSource p)
    Just (DetectedP d) -> do
      let fieldSel' = getFieldSel d
      let hReadMetadata' = getHReadMetadata d
      t <- send $ withBinaryFile p ReadMode $ \h -> do
        !t <- Fmt.tags <$> hReadMetadata' h
        return t
      let env = MetaEnv { fieldSel = fieldSel'
       , tags = t
       , source = FSSource p}
      return env

runMetaReadM :: forall effs a. LastMember IO effs =>
  Eff (MetaRead ': effs) a -> FilePath -> Eff effs a
runMetaReadM a p = do
  env <- readMetaEnvFromPath p
  runMetaReadPureM env a
