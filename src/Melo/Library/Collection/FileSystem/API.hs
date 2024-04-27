module Melo.Library.Collection.FileSystem.API where

import Control.Lens.At ()
import Control.Monad.Trans.Cont
import Data.List qualified as List
import Data.Text qualified as T
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Melo.Common.API
import Melo.Common.Exception
import Melo.Common.FileSystem
import Melo.Common.Monad
import Melo.Common.Routing
import Melo.Common.Uri
import Melo.Database.Repo as Repo
import Melo.Database.Repo.IO
import Melo.Library.Collection.Types
import Melo.Library.Source.Repo
import Melo.Library.Source.Types
import Rel8 qualified
import Rel8 ((&&.), (==.),)
import Streaming qualified as S
import Streaming.Prelude qualified as S
import System.FilePath
import System.Posix.Files

registerRoutes :: AppM IO IO ()
registerRoutes = do
  registerRoute (RouteKey "listEntries") (jsonRqJsonStreamRoute listEntries)
  pure ()

data ListEntries = ListEntries
  { collectionId :: CollectionRef
  , parent :: URI
  }
  deriving (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON JSONOptions ListEntries

data FileSystemEntry = FileSystemEntry
  { name :: Text
  , mtime :: UTCTime
  , type_ :: EntryType
  }
  deriving (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON JSONOptions FileSystemEntry
  deriving (TextShow) via FromGeneric FileSystemEntry

data EntryType = Directory | File
  deriving (Generic, Eq)
  deriving (FromJSON, ToJSON) via CustomJSON JSONOptions EntryType
  deriving (TextShow) via FromGeneric EntryType

instance From DirEntryType EntryType where
  from DirEntryDir = Directory
  from DirEntryFile = File

listEntries :: ListEntries -> ContT () (AppM IO IO) (S.Stream (S.Of FileSystemEntry) (AppM IO IO) ())
listEntries req =
  getSingle @CollectionEntity req.collectionId >>= \case
    Just collection | collection.root_uri `T.isPrefixOf` showt req.parent ->
      case addTrailingPathSeparator <$!> uriToFilePath req.parent of
        Nothing -> throwIO FileSystemNotBrowsable
        Just parent ->
          do
            uris <-
              selectStream do
                srcs <- orderByUri $ Rel8.each sourceSchema
                Rel8.where_ do
                  srcs.collection_id ==. Rel8.lit req.collectionId &&. srcs.source_uri `startsWith` Rel8.lit (T.pack $! addTrailingPathSeparator $! show req.parent)
                pure srcs.source_uri
            pure $! uris
              & S.mapMaybe (\uri ->
                parseURI uri.unpack >>= uriToFilePath >>= List.stripPrefix (dropTrailingPathSeparator parent)
                )
              & S.mapMaybe (\p -> splitDirectories p ^? ix 1)
              & S.nubOrd
              & S.mapMaybeM (\p -> do
                stat <- liftIO $ getFileStatus (parent </> p)
                let !mtime = modificationTimeHiRes stat
                let !t = if isRegularFile stat then
                            Just File
                          else if isDirectory stat then
                            Just Directory
                          else Nothing
                pure $! FileSystemEntry (T.pack p) (posixSecondsToUTCTime mtime) <$!> t
                )
              & dirsFirst
    _ -> throwIO CollectionNotFound
  where
    dirsFirst :: S.Stream (S.Of FileSystemEntry) (AppM IO IO) () -> S.Stream (S.Of FileSystemEntry) (AppM IO IO) ()
    dirsFirst es = do
      files <- S.toList_ $ S.partition (\e -> e.type_ == File) es
      S.each files

data FileSystemException =
    CollectionNotFound
  | FileSystemNotBrowsable
  deriving (Show, Exception)
