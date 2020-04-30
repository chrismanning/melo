module Main where

import Control.Monad
import Data.Coerce
import Data.Foldable
import Data.Generics.Labels ()
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Format
import GHC.OverloadedLabels
import Lens.Micro ((^.))
import Melo.Format.Info hiding (info)
import Melo.Format.Internal.Tag
import Melo.Format.Mapping
import Melo.Format.Metadata
import Options.Applicative
import Polysemy
import System.IO

data Opts = Opts
  { path :: FilePath
  }

detectOpts :: Parser Opts
detectOpts = Opts <$> strArgument (metavar "<audio file>")

main :: IO ()
main = detectAppM =<< execParser opts
  where
    opts =
      info
        (detectOpts <**> helper)
        ( fullDesc
            <> progDesc "Detect audio file type and print basic info"
            <> header "melo-detect - an audio file metadata reader"
        )

detectAppM :: Opts -> IO ()
detectAppM o = do
  f <- openMetadataFile (path o)
  printTags f

printTags :: MetadataFile -> IO ()
printTags f = do
  putLine
  psl "Info"
  putLine
  psl $ "File Path: " <> T.pack (f ^. #filePath)
  psl $ "File Type: " <> coerce (f ^. #fileId)
  let info = f ^. #audioInfo
  let channels = T.pack . show $ info ^. #channels
  psl $ "Channels: " <> channels
  let sampleRate = T.pack . show . samplesPerSecond $ info ^. #sampleRate
  psl $ "Sample Rate: " <> sampleRate <> "Hz"
  case info ^. #quality of
    Nothing -> pure ()
    Just quality -> psl $ "Quality: " <> quality
  case info ^. #bitsPerSample of
    Nothing -> pure ()
    Just bps -> psl $ "Sample Size: " <> T.pack (show bps) <> " bits"
  case audioLength info of
    Nothing -> pure ()
    Just len -> psl $ "Length: " <> T.pack (formatTime defaultTimeLocale "%-3Ess" len)
  forM_ (zip [1 ..] (toList (f ^. #metadata))) $ \(i, metadata) -> do
    putLine
    psl $ "Tags #" <> T.pack (show i)
    psl $ formatDesc metadata
    putLine
    let tags' = tags metadata
    let tag = lens metadata
    printTag "Track#" (tags' ^. tag trackNumber)
    printTag "Track Title" (tags' ^. tag trackTitle)
    printTag "Artist" (tags' ^. tag artist)
    printTag "Album" (tags' ^. tag album)
    printTag "Year" (tags' ^. tag year)
    printTag "Album Artist" (tags' ^. tag albumArtist)
    printTag "Genre" (tags' ^. tag genre)
  where
    putLine = psl (T.replicate 20 "-")
    psl = T.putStrLn
    printTag lbl t = psl $ formatTag lbl t
    formatTag l t = l <> T.pack ": " <> T.intercalate (T.pack " / ") (toList t)
