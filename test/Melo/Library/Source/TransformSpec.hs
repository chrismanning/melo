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
import Data.Coerce
import Data.Either
import Data.HashMap.Strict as H
import Data.List.NonEmpty as NE
import Data.Maybe
import Data.Text (Text)
import Data.UUID
import Data.UUID.V4
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Lens
import Melo.Common.FileSystem
import Melo.Library.Collection.FileSystem.WatchService
import Melo.Common.Logging
import Melo.Common.Metadata
import Melo.Common.Metadata.Mock
import Melo.Common.Uri
import Melo.Database.Repo.Fake
import Melo.Format.Flac
import Melo.Format.Info as F
import Melo.Format.Mapping
import Melo.Format.Metadata as F
import Melo.Format.Vorbis
import Melo.Library.Collection.Repo.Fake
import Melo.Library.Collection.Service
import Melo.Library.Collection.Types qualified as Ty
import Melo.Library.Source.Repo.Fake
import Melo.Library.Source.Transform qualified as SUT
import Melo.Library.Source.Types (Source (..))
import Melo.Library.Source.Types as Ty
import Melo.Lookup.MusicBrainz.Mock
import Melo.Metadata.Mapping.Repo.Fake
import Melo.Metadata.Mapping.Types
import Test.Hspec
import Witch

spec :: Spec
spec = do
  describe "Source Transformations" $ do
    let collectionRef = Ty.CollectionRef $ fromWords64 1 1
    let collectionRepo =
          FakeRepository $
            H.fromList
              [ ( collectionRef,
                  Ty.CollectionTable
                    { id = collectionRef,
                      root_uri = "file:/music",
                      name = "music",
                      watch = False,
                      kind = "filesystem"
                    }
                )
              ]
    let tagMappingRepo =
          mkTagMappingRepo
            [ ("artist", artist),
              ("album_artist", albumArtist),
              ("year", year),
              ("album", album),
              ("track_number", trackNumber),
              ("track_title", trackTitle)
            ]
    context "moving files by patterns" $ do
      let patterns =
            NE.fromList
              [ Ty.MappingPattern "album_artist",
                Ty.LiteralPattern "/",
                Ty.GroupPattern (Ty.MappingPattern "year" :| [Ty.LiteralPattern " - "]),
                Ty.MappingPattern "album",
                Ty.LiteralPattern "/",
                Ty.GroupPattern (Ty.MappingPattern "track_number" :| [Ty.LiteralPattern " - "]),
                Ty.MappingPattern "track_title"
              ]
      it "transforms with pattern" $ do
        let tags =
              [ ("ARTIST", "Artist"),
                ("ALBUM", "Album Title"),
                ("TRACKNUMBER", "01"),
                ("TITLE", "Track Title"),
                ("DATE", "1999")
              ]
        let orig = mkSource collectionRef tags
        let fixture =
              TransformFixture
                { transforms = [moveByPatterns patterns],
                  sourceRepo = mkSourceRepo [orig],
                  collectionRepo,
                  tagMappingRepo,
                  metadataServiceActions = [],
                  musicBrainzServiceActions = []
                }
        let expected = orig & #source .~ parseUriUnsafe "file:/music/Artist/1999%20-%20Album%20Title/01%20-%20Track%20Title.flac"
        previewTransformationsFixture fixture (V.singleton orig) `shouldReturn` V.singleton expected
      it "transforms with grouped pattern when tags missing date" $ do
        let tags =
              [ ("ARTIST", "Artist"),
                ("ALBUM", "Album Title"),
                ("TRACKNUMBER", "01"),
                ("TITLE", "Track Title")
              ]
        let orig = mkSource collectionRef tags
        let fixture =
              TransformFixture
                { transforms = [moveByPatterns patterns],
                  sourceRepo = mkSourceRepo [orig],
                  collectionRepo,
                  tagMappingRepo,
                  metadataServiceActions = [],
                  musicBrainzServiceActions = []
                }
        let expected = orig & #source .~ parseUriUnsafe "file:/music/Artist/Album%20Title/01%20-%20Track%20Title.flac"
        previewTransformationsFixture fixture (V.singleton orig) `shouldReturn` V.singleton expected
      it "transforms with cover image" $ do
        let tags =
              [ ("ARTIST", "Artist"),
                ("ALBUM", "Album Title"),
                ("TRACKNUMBER", "01"),
                ("TITLE", "Track Title")
              ]
        let orig = mkSource collectionRef tags
        let fixture =
              TransformFixture
                { transforms = [moveByPatterns patterns],
                  sourceRepo = mkSourceRepo [orig],
                  collectionRepo,
                  tagMappingRepo,
                  metadataServiceActions = [],
                  musicBrainzServiceActions = []
                }
        let expected = orig & #source .~ parseUriUnsafe "file:/music/Artist/Album%20Title/01%20-%20Track%20Title.flac"
        previewTransformationsFixture fixture (V.singleton orig) `shouldReturn` V.singleton expected
    context "editing metadata" $ do
      let metadataFile =
            F.MetadataFile
              { metadata = H.empty,
                fileId = flacFileId,
                filePath = "test.flac",
                audioInfo =
                  F.Info
                    { channels = F.Stereo,
                      sampleRate = F.SampleRate 44100,
                      bitsPerSample = Just 16,
                      totalSamples = Nothing,
                      quality = Nothing
                    }
              }
      it "removes single mapping" $ do
        let origTags =
              [ ("ARTIST", "Artist"),
                ("ALBUM", "Album Title"),
                ("TRACKNUMBER", "01"),
                ("TITLE", "Track Title"),
                ("DATE", "1999")
              ]
        let orig = mkSource collectionRef origTags
        let tags =
              [ ("ALBUM", "Album Title"),
                ("TRACKNUMBER", "01"),
                ("TITLE", "Track Title"),
                ("DATE", "1999")
              ]
        let expected = mkSource collectionRef tags
        let fixture =
              TransformFixture
                { transforms = [transformEditMetadata (SUT.RemoveMappings ["artist"])],
                  sourceRepo = mkSourceRepo [orig],
                  collectionRepo,
                  tagMappingRepo,
                  metadataServiceActions =
                    [ ReadMetadataFile flacFileId metadataFile.filePath :-> Right metadataFile
                    --            WriteMetadataFile metadataFile (metadataFile ^. #filePath) :-> Right metadataFile
                    ],
                  musicBrainzServiceActions = []
                }
        previewTransformationsFixture fixture (V.singleton orig) >>= (`shouldSatisfy` \actual -> (actual V.! 0 & #ref .~ orig.ref) == expected)
      it "retains mappings" $ do
        let origTags =
              [ ("ARTIST", "Artist"),
                ("ALBUM", "Album Title"),
                ("TRACKNUMBER", "01"),
                ("TITLE", "Track Title"),
                ("DATE", "1999"),
                ("COMMENT", "sjkfnks")
              ]
        let orig = mkSource collectionRef origTags
        let fixture =
              TransformFixture
                { transforms = [transformEditMetadata (SUT.Retain ["artist", "album"])],
                  sourceRepo = mkSourceRepo [orig],
                  collectionRepo,
                  tagMappingRepo,
                  metadataServiceActions =
                    [ ReadMetadataFile flacFileId metadataFile.filePath :-> Right metadataFile
                    ],
                  musicBrainzServiceActions = []
                }
        let tags =
              [ ("ARTIST", "Artist"),
                ("ALBUM", "Album Title")
              ]
        let expected = mkSource collectionRef tags
        previewTransformationsFixture fixture (V.singleton orig) >>= (`shouldSatisfy` \actual -> (actual V.! 0 & #ref .~ orig.ref) == expected)
  describe "Pattern Parsing" $ do
    it "parses literal patterns from text" $ do
      SUT.parseMovePattern " " `shouldBe` Right (NE.singleton (Ty.LiteralPattern " "))
      SUT.parseMovePattern "liter al" `shouldBe` Right (NE.singleton (Ty.LiteralPattern "liter al"))
    it "parses group pattern from text" $ do
      SUT.parseMovePattern "[a]" `shouldBe` Right (NE.singleton (Ty.GroupPattern (Ty.LiteralPattern "a" :| [])))
      SUT.parseMovePattern "[" `shouldSatisfy` isLeft
    it "parses nested group pattern from text" $
      SUT.parseMovePattern "[a[ b]c]"
        `shouldBe` Right
          ( Ty.GroupPattern
              ( Ty.LiteralPattern "a"
                  :| [ Ty.GroupPattern (Ty.LiteralPattern " b" :| []),
                       Ty.LiteralPattern "c"
                     ]
              )
              :| []
          )
    it "parses tag mapping" $ do
      SUT.parseMovePattern "%album_artist" `shouldBe` Right (Ty.MappingPattern "album_artist" :| [])
      SUT.parseMovePattern "%02track_number" `shouldBe` Right (Ty.PrintfPattern "%02s" (Ty.MappingPattern "track_number") :| [])
    it "parses tag mapping in group" $ do
      SUT.parseMovePattern "test[%track_title" `shouldSatisfy` isLeft
      SUT.parseMovePattern "test[%track_title]" `shouldBe` Right (Ty.LiteralPattern "test" :| [Ty.GroupPattern (Ty.MappingPattern "track_title" :| [])])
    it "parses example" $
      SUT.parseMovePattern "%album_artist[ %artist_origin]/%original_release_year - %album_title/%track_number - %track_title"
        `shouldBe` Right
          ( Ty.MappingPattern "album_artist"
              :| [ Ty.GroupPattern (Ty.LiteralPattern " " :| [Ty.MappingPattern "artist_origin"]),
                   Ty.LiteralPattern "/",
                   Ty.MappingPattern "original_release_year",
                   Ty.LiteralPattern " - ",
                   Ty.MappingPattern "album_title",
                   Ty.LiteralPattern "/",
                   Ty.MappingPattern "track_number",
                   Ty.LiteralPattern " - ",
                   Ty.MappingPattern "track_title"
                 ]
          )

type TestTransformer = V.Vector Ty.Source -> TestTransformT (V.Vector Ty.Source)

type TestTransformT =
  SUT.TransformPreviewT
    ( FakeTagMappingRepositoryT
        ( FakeSourceRepositoryT
            ( FakeCollectionRepositoryT
                ( MockT
                    MusicBrainzServiceAction
                    ( MockT
                        MetadataAggregateAction
                        ( FileSystemWatcherIOT
                            (FileSystemIOT
                                (LoggingIOT IO)
                            )
                        )
                    )
                )
            )
        )
    )

instance (
  MonadIO m,
  MonadCatch m,
  MonadThrow m,
  MonadMask m,
  MonadSTM (STM (MockT a m)),
  Ord (ThreadId (MockT a m)),
  Show (ThreadId (MockT a m))
  ) => MonadConc (MockT a m) where

data Fixture m f t =
    MockFixture [WithResult m]
  | FakeFixture f
  | RealFixture t

data TransformFixture = TransformFixture
  { collectionRepo :: FakeCollectionRepository,
    sourceRepo :: FakeSourceRepository,
    tagMappingRepo :: FakeTagMappingRepository,
    metadataServiceActions :: [WithResult MetadataAggregateAction],
    musicBrainzServiceActions :: [WithResult MusicBrainzServiceAction],
    transforms :: [TestTransformer]
  }

previewTransformationsFixture :: TransformFixture -> V.Vector Ty.Source -> IO (V.Vector Ty.Source)
previewTransformationsFixture TransformFixture {..} ss = runSourceTransform $ SUT.previewTransformations transforms ss
  where
    runSourceTransform =
      runStdoutLogging
        . runFileSystemIO
        . runFileSystemWatcherIO _ _
        . runMockT metadataServiceActions
        . runMockT musicBrainzServiceActions
        . runFakeCollectionRepository collectionRepo
        . runFakeSourceRepository sourceRepo
        . runFakeTagMappingRepository tagMappingRepo

moveByPatterns :: NonEmpty Ty.SourcePathPattern -> TestTransformer
moveByPatterns patterns = SUT.evalTransformAction (SUT.Move Nothing patterns)

transformEditMetadata :: SUT.MetadataTransformation -> TestTransformer
transformEditMetadata mt = SUT.evalTransformAction (SUT.EditMetadata mt)

parseUriUnsafe :: String -> URI
parseUriUnsafe s = case parseURI s of
  Just uri -> uri
  Nothing -> error ("invalid uri " <> s)

mkSource :: Ty.CollectionRef -> [(Text, Text)] -> Source
mkSource collectionRef tags =
  let Just metadata = mkMetadata vorbisCommentsId (Tags $ V.fromList tags)
      srcRef = Ty.SourceRef $ fromWords64 2 2
   in Ty.Source
        { ref = srcRef,
          source = parseUriUnsafe "file:test.flac",
          kind = flacFileId,
          multiTrack = Nothing,
          collectionRef,
          length = Nothing,
          cover = Nothing,
          metadata
        }

mkSourceRepo :: [Ty.Source] -> FakeSourceRepository
mkSourceRepo ss =
  FakeRepository $
    H.fromList (fmap (\s -> s.ref, from s)) ss)

mkTagMappingRepo :: [(Text, TagMapping)] -> FakeTagMappingRepository
mkTagMappingRepo tm =
  FakeRepository $
    H.fromList (fmap (\(k, v) -> (k, from (NewTagMapping k (coerce v)))) tm)
