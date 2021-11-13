{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Source.TransformSpec where

import Control.Concurrent.Classy
import Control.Exception (evaluate)
import Control.Exception.Safe
import Control.Lens hiding (from)
import Control.Monad
import Control.Monad.Base
import Control.Monad.Mock
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Data.HashMap.Strict as H
import Data.List.NonEmpty as NE
import Data.Maybe
import Melo.Database.Repo.Fake
import Data.UUID
import Data.UUID.V4
import qualified Data.Vector as V
import Melo.Format.Flac
import Melo.Format.Mapping
import Melo.Format.Vorbis
import Melo.Library.Collection.Service
import Melo.Library.Collection.Repo.Fake
import qualified Melo.Library.Collection.Types as Ty
import Melo.Library.Source.Repo.Fake
import qualified Melo.Library.Source.Transform as SUT
import qualified Melo.Library.Source.Types as Ty
import Melo.Library.Source.Types (Source(..))
import Melo.Common.FileSystem
import Melo.Common.Logging
import Melo.Common.Metadata
import Melo.Common.Metadata.Mock
import Melo.Common.Uri
import Melo.Format.Metadata
import Melo.Lookup.MusicBrainz.Mock
import Witch

import Test.Hspec

spec :: Spec
spec =
  describe "Source Transformations" $
    context "with file move patterns" $ do
      let collectionRef = Ty.CollectionRef $ fromWords64 1 1
      let collectionRepo = FakeRepository $
             H.fromList [(collectionRef, Ty.CollectionTable {
               id = collectionRef,
               root_uri = "file:/music",
               name = "music",
               watch = False,
               kind = "filesystem"
             })]
      let patterns = NE.fromList [
            Ty.MappingPattern albumArtist,
            Ty.LiteralPattern "/",
            Ty.GroupPattern (Ty.MappingPattern year :| [Ty.LiteralPattern " - "]),
            Ty.MappingPattern album,
            Ty.LiteralPattern "/",
            Ty.GroupPattern (Ty.MappingPattern trackNumber :| [Ty.LiteralPattern " - "]),
            Ty.MappingPattern trackTitle
            ]
      it "transforms with pattern" $ do
        let tags = [
              ("ARTIST", "Artist"),
              ("ALBUM", "Album Title"),
              ("TRACKNUMBER", "01"),
              ("TITLE", "Track Title"),
              ("DATE", "1999")
              ]
        let orig = mkSource collectionRef tags
        let fixture = TransformFixture {
          transforms = [moveByPatterns patterns],
          sourceRepo = mkSourceRepo [orig],
          collectionRepo
        }
        let expected = orig {
            source = parseUriUnsafe "file:/music/Artist/1999%20-%20Album%20Title/01%20-%20Track%20Title.flac"
          }
        previewTransformationsFixture fixture [orig] `shouldReturn` [expected]
      it "transforms with grouped pattern when tags missing date" $ do
        let tags = [
              ("ARTIST", "Artist"),
              ("ALBUM", "Album Title"),
              ("TRACKNUMBER", "01"),
              ("TITLE", "Track Title")
              ]
        let orig = mkSource collectionRef tags
        let fixture = TransformFixture {
          transforms = [moveByPatterns patterns],
          sourceRepo = mkSourceRepo [orig],
          collectionRepo
        }
        let expected = orig {
            source = parseUriUnsafe "file:/music/Artist/Album%20Title/01%20-%20Track%20Title.flac"
          }
        previewTransformationsFixture fixture [orig] `shouldReturn` [expected]

type TestTransformer = [Ty.Source] -> TestTransformT [Ty.Source]

type TestTransformT = SUT.FileSystemPreviewT (
    FakeSourceRepositoryT (
      FakeCollectionRepositoryT (
        MockT MusicBrainzServiceAction (
          MockT MetadataServiceAction (
            FileSystemIOT (LoggingIOT IO)
          )
        )
      )
    )
  )

data TransformFixture = TransformFixture {
  collectionRepo :: FakeCollectionRepository,
  sourceRepo :: FakeSourceRepository,
  transforms :: [TestTransformer]
}

previewTransformationsFixture :: TransformFixture -> [Ty.Source] -> IO [Ty.Source]
previewTransformationsFixture TransformFixture{..} ss = runSourceTransform $ SUT.previewTransformations transforms ss
  where
    runSourceTransform =
      runStdoutLogging .
      runFileSystemIO .
      runMockT ([] :: [WithResult MetadataServiceAction]) .
      runMockT ([] :: [WithResult MusicBrainzServiceAction]) .
      runFakeCollectionRepository collectionRepo .
      runFakeSourceRepository sourceRepo

moveByPatterns :: NonEmpty Ty.SourcePathPattern -> TestTransformer
moveByPatterns patterns ss = forM ss $ \src ->
  moveSourceWithPattern patterns src >>= \case
    Left e -> do
      liftIO $ putStrLn (show e)
      $(logInfoIO) $ show e
      pure src
    Right uri -> pure $ src & #source .~ uri

parseUriUnsafe :: String -> URI
parseUriUnsafe s = case parseURI s of
  Just uri -> uri
  Nothing -> error ("invalid uri " <> s)

mkSource collectionRef tags =
  let Just metadata = mkMetadata vorbisCommentsId (Tags $ V.fromList tags)
      srcRef = Ty.SourceRef $ fromWords64 2 2 in
          Ty.Source {
            ref = srcRef,
            source = parseUriUnsafe "file:test.flac",
            kind = MetadataFileId "Flac",
            range = Nothing,
            collectionRef,
            metadata
          }

mkSourceRepo :: [Ty.Source] -> FakeSourceRepository
mkSourceRepo ss = FakeRepository $
  H.fromList (fmap (\s -> (s ^. #ref, from s)) ss)
