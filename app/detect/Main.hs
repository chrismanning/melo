module Main where

import Control.Monad.Freer
import Data.Text as T
import qualified Data.Text.IO as T
import Options.Applicative

import Melo.Mapping
import Melo.Metadata as M

data Opts = Opts {
  path :: FilePath
}

detectOpts :: Parser Opts
detectOpts = Opts <$> strArgument (metavar "<audio file>")

main :: IO ()
main = detectAppM =<< execParser opts
         where
           opts = info (detectOpts <**> helper)
             ( fullDesc
            <> progDesc "Detect audio file type and print basic info"
            <> header "melo-detect - an audio file metadata reader" )

detectAppM :: Opts -> IO ()
detectAppM o = M.runMetaReadFromPath printTags (path o)

printTags :: (Member IO effs, Member MetaRead effs) => Eff effs ()
printTags = do
  printTag "Track#" trackNumber
  printTag "Track Title" trackTitle
  printTag "Artist" artist
  printTag "Album" album
  printTag "Year" year
  printTag "Album Artist" albumArtist
    where
      printTag lbl m = formatTag lbl <$> readTag m >>= send . T.putStrLn
      formatTag l t = l <> T.pack ": " <> T.intercalate (T.pack " / ") t
