module Main where

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Data.Time.Format
import           Options.Applicative
import           Polysemy
import           System.IO

import           Melo.Format.Info                  hiding ( info )
import           Melo.Format.Mapping
import           Melo.Format.Tag

data Opts = Opts {
  path :: FilePath
}

detectOpts :: Parser Opts
detectOpts = Opts <$> strArgument (metavar "<audio file>")

main :: IO ()
main = detectAppM =<< execParser opts
 where
  opts = info
    (detectOpts <**> helper)
    (  fullDesc
    <> progDesc "Detect audio file type and print basic info"
    <> header "melo-detect - an audio file metadata reader"
    )

detectAppM :: Opts -> IO ()
detectAppM o = withBinaryFile (path o) ReadMode $ \h ->
  runM $ (hRunTagReadM h . hRunInfoReadM h) printTags

printTags :: (Members '[Embed IO, TagRead, InfoRead] effs) => Sem effs ()
printTags = do
  putLine
  psl "Info"
  putLine
  channels <- T.pack . show <$> readChannels
  psl $ "Channels: " <> channels
  sampleRate <- T.pack . show . samplesPerSecond <$> readSampleRate
  psl $ "Sample Rate: " <> sampleRate <> "Hz"
  readBitsPerSample >>= \case
    Nothing  -> pure ()
    Just bps -> psl $ "Sample Size: " <> T.pack (show bps) <> " bits"
  readAudioLength >>= \case
    Nothing -> pure ()
    Just len ->
      psl $ "Length: " <> T.pack (formatTime defaultTimeLocale "%-3Ess" len)

  putLine
  psl "Tags"
  putLine
  printTag "Track#"       trackNumber
  printTag "Track Title"  trackTitle
  printTag "Artist"       artist
  printTag "Album"        album
  printTag "Year"         year
  printTag "Album Artist" albumArtist
 where
  putLine = psl (T.replicate 20 "-")
  psl     = embed . T.putStrLn
  printTag lbl m = formatTag lbl <$> readTag m >>= psl
  formatTag l t = l <> T.pack ": " <> T.intercalate (T.pack " / ") t
