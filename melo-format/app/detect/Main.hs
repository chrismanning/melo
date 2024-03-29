module Main where

import Control.Monad
import Control.Monad.Extra (whenJust)
import Data.Coerce
import Data.Foldable
import Data.Generics.Labels ()
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Format
import Lens.Micro ((^.))
import Melo.Format.Info hiding (info)
import Melo.Format.Mapping
import Melo.Format.Metadata
import Options.Applicative

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
  psl $ "File Path: " <> T.pack f.filePath
  psl $ "File Type: " <> coerce f.fileId
  let info = f.audioInfo
  let channels = T.pack . show $ info.channels
  psl $ "Channels: " <> channels
  let sampleRate = T.pack $ show $ samplesPerSecond info.sampleRate
  psl $ "Sample Rate: " <> sampleRate <> "Hz"
  whenJust info.quality $ \quality ->
    psl $ "Quality: " <> quality
  whenJust info.bitsPerSample $ \bps ->
    psl $ "Sample Size: " <> T.pack (show bps) <> " bits"
  whenJust (audioLength info) $ \len ->
    psl $ "Length: " <> T.pack (formatTime defaultTimeLocale "%-3Ess" len)
  forM_ (toList f.metadata) $ \metadata -> do
    putLine
    psl $ "Tags - " <> metadata.formatDesc
    putLine
    printTag "Track#" (metadata.tagHead trackNumber)
    printTag "Track Title" (metadata.tagHead trackTitle)
    printTag "Artist" (metadata.tag artist)
    printTag "Album" (metadata.tagHead album)
    printTag "Year" (metadata.tagHead year)
    printTag "Album Artist" (metadata.tag albumArtist)
    printTag "Genre" (metadata.tag genre)
  where
    putLine = psl (T.replicate 20 "-")
    psl = T.putStrLn
    printTag lbl t = psl $ formatTag lbl t
    formatTag l t = l <> T.pack ": " <> T.intercalate (T.pack " / ") (toList t)
