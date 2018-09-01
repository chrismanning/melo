module Main where

import Data.Text as T
import qualified Data.Text.IO as T
import Options.Applicative

import Melo.Detect
import Melo.Format
import Melo.Mapping

data Opts = Opts {
  path :: FilePath
}

detectOpts :: Parser Opts
detectOpts = Opts <$> strArgument (metavar "<audio file>")

main :: IO ()
main = detect =<< execParser opts
         where
           opts = info (detectOpts <**> helper)
             ( fullDesc
            <> progDesc "Detect audio file type and print basic info"
            <> header "melo-detect - an audio file metadata reader" )

detect :: Opts -> IO ()
detect o = let p = path o in do
  Just (Detected readMetadata sel) <- return $ detectFile p
  t <- tags <$> readMetadata p
  let getTag = \m -> getMappedTag sel m t
  printTag "Track#" (getTag trackNumber)
  printTag "Track Title" (getTag trackTitle)
  printTag "Artist" (getTag artist)
  printTag "Album" (getTag album)
  printTag "Year" (getTag year)
  printTag "Album Artist" (getTag albumArtist)

printTag :: String -> [Text] -> IO ()
printTag l v = do
  putStr $ l ++ ": "
  T.putStrLn $ T.intercalate (T.pack " / ") v
