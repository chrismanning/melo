module Main where

import Codec.Archive.Zip
import Control.Lens ((^.), (^?), _head)
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Lazy as L
import Data.Functor.Identity (runIdentity)
import GHC.Generics
import Network.HTTP.Types.Header
import Network.Parser.Rfc2183
import Network.Wreq
import Options.Harg
import System.Directory
import System.FilePath

newtype Config = Config
  { _bandcampDlUrl :: String
  }
  deriving stock (Show, Generic)

configOpt :: HKD Config Opt
configOpt =
  build @Config bandcampDlUrl
  where
    bandcampDlUrl :: Opt String
    bandcampDlUrl =
      argument
        strParser
        ( metavar "BANDCAMP_DL_URL"
            . help "Bandcamp download URL"
        )

main :: IO ()
main = do
  result <- execOptDef configOpt
  let config = runIdentity $ construct result
  print config
  let url = _bandcampDlUrl config
  response <- get url
  case response ^? responseHeader hContentDisposition >>= getFilename of
    Just filename -> do
      L.writeFile filename (response ^. responseBody)
      if takeExtension filename == ".zip"
        then do
          let dest = dropExtension filename
          withArchive filename (unpackInto dest)
          removeFile filename
        else putStrLn $ "Downloaded " <> filename
    Nothing -> putStrLn "Unknown filename"
  pure ()

getFilename :: ByteString -> Maybe String
getFilename s = case parseOnly dispositionParser s of
  Left _ -> error "Could not parse Content-Disposition header"
  Right disposition ->
    if dispositionType disposition == Attachment
      then unpack <$> [filename | Filename filename <- dispositionParameters disposition] ^? _head
      else Nothing
