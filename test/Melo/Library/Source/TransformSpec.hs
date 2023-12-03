{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Source.TransformSpec where

import Control.Concurrent.Classy
import Control.Exception (evaluate)
import Control.Lens hiding (from)
import Control.Monad
import Control.Monad.Base
import Control.Monad.Mock
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.State.Strict
import Data.Coerce
import Data.Either.Combinators
import Data.HashMap.Strict as H
import Data.List.NonEmpty as NE
import Data.Maybe
import Data.Text (Text)
import Data.UUID
import Data.UUID.V4
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Lens
import Melo.Common.Exception
import Melo.Common.FileSystem
import Melo.Common.FileSystem.Watcher
import Melo.Common.Logging
import Melo.Metadata.Aggregate
import Melo.Metadata.Aggregate.Mock
import Melo.Common.Uri
import Melo.Database.Repo.Fake
import Melo.Format.Flac
import Melo.Format.Info as F
import Melo.Format.Mapping as F
import Melo.Format.Metadata as F
import Melo.Format.Vorbis
import Melo.Library.Artist.Name.Repo.Fake
import Melo.Library.Artist.Name.Types qualified as Ty
import Melo.Library.Artist.Repo.Fake
import Melo.Library.Artist.Types qualified as Ty
import Melo.Library.Collection.Repo.Fake
import Melo.Library.Collection.Aggregate
import Melo.Library.Collection.Types qualified as Ty
import Melo.Library.Release.ArtistName.Repo.Fake
import Melo.Library.Release.ArtistName.Types qualified as Ty
import Melo.Library.Release.Types qualified as Ty
import Melo.Library.Source.Repo.Fake
import Melo.Library.Source.Transform qualified as SUT
import Melo.Library.Source.Types (Source (..))
import Melo.Library.Source.Types as Ty
import Melo.Library.Track.Repo.Fake
import Melo.Library.Track.Types qualified as Ty
import Melo.Lookup.Covers.Mock
import Melo.Lookup.MusicBrainz.Mock
import Melo.Metadata.Mapping.Repo.Fake
import Melo.Metadata.Mapping.Types
import Rel8 qualified
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
                      kind = "filesystem",
                      rescan = False
                    }
                )
              ]
    let tagMappingRepo =
          mkTagMappingRepo
            [ ("artist", artist),
              ("release_artist", albumArtist),
              ("year", year),
              ("album", album),
              ("track_number", trackNumber),
              ("track_title", trackTitle)
            ]
    let trackRepo = mkTrackRepo []
    let artistNameRepo = mkArtistNameRepo []
    let releaseArtistNameRepo = mkReleaseArtistNameRepo []
    let artistRepo = mkArtistRepo
    context "moving files by patterns" $ do
      let patterns =
            NE.fromList
              [ Ty.MappingPattern "release_artist",
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
                { transform = moveByPatterns patterns,
                  sourceRepo = mkSourceRepo [orig],
                  collectionRepo,
                  tagMappingRepo,
                  trackRepo,
                  artistNameRepo,
                  releaseArtistNameRepo,
                  artistRepo,
                  metadataAggregateActions = [],
                  musicBrainzServiceActions = [],
                  coverServiceActions = []
                }
        let expected = orig & #source .~ parseUriUnsafe "file:/music/Artist/1999%20-%20Album%20Title/01%20-%20Track%20Title.flac"
        (rightToMaybe <$> previewTransformationFixture fixture orig) `shouldReturn` Just expected
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
                { transform = moveByPatterns patterns,
                  sourceRepo = mkSourceRepo [orig],
                  collectionRepo,
                  tagMappingRepo,
                  trackRepo,
                  artistNameRepo,
                  releaseArtistNameRepo,
                  artistRepo,
                  metadataAggregateActions = [],
                  musicBrainzServiceActions = [],
                  coverServiceActions = []
                }
        let expected = orig & #source .~ parseUriUnsafe "file:/music/Artist/Album%20Title/01%20-%20Track%20Title.flac"
        (rightToMaybe <$> previewTransformationFixture fixture orig) `shouldReturn` Just expected
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
                { transform = moveByPatterns patterns,
                  sourceRepo = mkSourceRepo [orig],
                  collectionRepo,
                  tagMappingRepo,
                  trackRepo,
                  artistNameRepo,
                  releaseArtistNameRepo,
                  artistRepo,
                  metadataAggregateActions = [],
                  musicBrainzServiceActions = [],
                  coverServiceActions = []
                }
        let expected = orig & #source .~ parseUriUnsafe "file:/music/Artist/Album%20Title/01%20-%20Track%20Title.flac"
        (rightToMaybe <$> previewTransformationFixture fixture orig) `shouldReturn` Just expected
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
                    },
                pictures = []
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
                { transform = transformEditMetadata (SUT.RemoveMappings $ V.fromList ["artist"]),
                  sourceRepo = mkSourceRepo [orig],
                  collectionRepo,
                  tagMappingRepo,
                  trackRepo,
                  artistNameRepo,
                  releaseArtistNameRepo,
                  artistRepo,
                  metadataAggregateActions =
                    [ ReadMetadataFile flacFileId metadataFile.filePath :-> Right metadataFile
                    --            WriteMetadataFile metadataFile (metadataFile ^. #filePath) :-> Right metadataFile
                    ],
                  musicBrainzServiceActions = [],
                  coverServiceActions = []
                }
        (rightToMaybe <$> previewTransformationFixture fixture orig) >>= (`shouldSatisfy` \actual -> (actual <&> #ref .~ orig.ref) == Just expected)
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
                { transform = transformEditMetadata (SUT.RetainMappings $ V.fromList ["artist", "album"]),
                  sourceRepo = mkSourceRepo [orig],
                  collectionRepo,
                  tagMappingRepo,
                  trackRepo,
                  artistNameRepo,
                  releaseArtistNameRepo,
                  artistRepo,
                  metadataAggregateActions =
                    [ ReadMetadataFile flacFileId metadataFile.filePath :-> Right metadataFile
                    ],
                  musicBrainzServiceActions = [],
                  coverServiceActions = []
                }
        let tags =
              [ ("ARTIST", "Artist"),
                ("ALBUM", "Album Title")
              ]
        let expected = mkSource collectionRef tags
        (rightToMaybe <$> previewTransformationFixture fixture orig) >>= (`shouldSatisfy` \actual -> (actual <&> #ref .~ orig.ref) == Just expected)
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
      SUT.parseMovePattern "%release_artist" `shouldBe` Right (Ty.MappingPattern "release_artist" :| [])
      SUT.parseMovePattern "%02track_number" `shouldBe` Right (Ty.PrintfPattern "%02s" (Ty.MappingPattern "track_number") :| [])
    it "parses tag mapping in group" $ do
      SUT.parseMovePattern "test[%track_title" `shouldSatisfy` isLeft
      SUT.parseMovePattern "test[%track_title]" `shouldBe` Right (Ty.LiteralPattern "test" :| [Ty.GroupPattern (Ty.MappingPattern "track_title" :| [])])
    it "parses example" $
      SUT.parseMovePattern "%release_artist[ %artist_origin]/%original_release_year - %release_title/%track_number - %track_title"
        `shouldBe` Right
          ( Ty.MappingPattern "release_artist"
              :| [ Ty.GroupPattern (Ty.LiteralPattern " " :| [Ty.MappingPattern "artist_origin"]),
                   Ty.LiteralPattern "/",
                   Ty.MappingPattern "original_release_year",
                   Ty.LiteralPattern " - ",
                   Ty.MappingPattern "release_title",
                   Ty.LiteralPattern "/",
                   Ty.MappingPattern "track_number",
                   Ty.LiteralPattern " - ",
                   Ty.MappingPattern "track_title"
                 ]
          )

type TestTransformer = Ty.Source -> TestTransformT (Either SUT.TransformationError Ty.Source)

type TestTransformT =
  SUT.TransformPreviewT
      (SUT.VirtualArtistRepoT
          (StateT SUT.VirtualEntities
              ( FakeArtistRepositoryT
                  ( FakeReleaseArtistNameRepositoryT
                      ( FakeArtistNameRepositoryT
                          ( FakeTrackRepositoryT
                              ( FakeTagMappingRepositoryT
                                  ( FakeSourceRepositoryT
                                      ( FakeCollectionRepositoryT
                                          ( MockT
                                              CoverServiceAction
                                              ( MockT
                                                  MusicBrainzServiceAction
                                                  ( MockT
                                                      MetadataAggregateAction
                                                      IO
                                                  )
                                              )
                                          )
                                      )
                                  )
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
    trackRepo :: FakeTrackRepository,
    artistNameRepo :: FakeArtistNameRepository,
    releaseArtistNameRepo :: FakeReleaseArtistNameRepository,
    artistRepo :: FakeArtistRepository,
    metadataAggregateActions :: [WithResult MetadataAggregateAction],
    musicBrainzServiceActions :: [WithResult MusicBrainzServiceAction],
    coverServiceActions :: [WithResult CoverServiceAction],
    transform :: TestTransformer
  }

previewTransformationFixture :: TransformFixture -> Ty.Source -> IO (Either SUT.TransformationError Ty.Source)
previewTransformationFixture TransformFixture {..} ss = runSourceTransform $ SUT.previewTransformation transform ss
  where
    runSourceTransform =
--      runStdoutLogging
--        . runFileSystemIO
--        . runFileSystemWatcherIO _ _
        runMockT metadataAggregateActions
        . runMockT musicBrainzServiceActions
        . runMockT coverServiceActions
        . runFakeCollectionRepository collectionRepo
        . runFakeSourceRepository sourceRepo
        . runFakeTagMappingRepository tagMappingRepo
        . runFakeTrackRepository trackRepo
        . runFakeArtistNameRepository artistNameRepo
        . runFakeReleaseArtistNameRepository releaseArtistNameRepo
        . runFakeArtistRepository artistRepo

moveByPatterns :: NonEmpty Ty.SourcePathPattern -> TestTransformer
moveByPatterns patterns = SUT.evalTransformAction (SUT.Move Nothing patterns)

transformEditMetadata :: SUT.MetadataTransformation -> TestTransformer
transformEditMetadata mt = SUT.evalTransformAction (SUT.EditMetadata (V.singleton mt))

parseUriUnsafe :: String -> URI
parseUriUnsafe s = case parseURI s of
  Just uri -> uri
  Nothing -> error ("invalid uri " <> s)

mkSource :: Ty.CollectionRef -> [(Text, Text)] -> Source
mkSource collectionRef tags =
  let metadata = mkMetadata vorbisCommentsId (Tags $ V.fromList tags)
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
    H.fromList (fmap (\s -> (s.ref, from s)) ss)

mkTagMappingRepo :: [(Text, F.TagMapping)] -> FakeTagMappingRepository
mkTagMappingRepo tm =
  FakeRepository $
    H.fromList (fmap (\(k, v) -> (k, from (fromTagMapping k v))) tm)

mkTrackRepo :: [Ty.Track] -> FakeTrackRepository
mkTrackRepo ts =
  FakeRepository $
    H.fromList (fmap (\t -> (t.ref, from t)) ts)

mkArtistNameRepo :: [Ty.ArtistNameEntity] -> FakeArtistNameRepository
mkArtistNameRepo ns =
  FakeRepository $
    H.fromList (fmap (\n -> (n.id, from n)) ns)

mkReleaseArtistNameRepo :: [(Ty.ReleaseRef, [Ty.ArtistNameEntity])] -> FakeReleaseArtistNameRepository
mkReleaseArtistNameRepo ns =
  FakeReleaseArtistNameRepository $
    H.fromList (fmap (\(k, v) -> (k, V.fromList v)) ns)

mkArtistRepo :: FakeArtistRepository
mkArtistRepo =
  FakeRepository mempty
