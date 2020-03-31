module Melo.Library.Artist.ServiceSpec where

import Control.Carrier.Reader
import Control.Monad
import Melo.Common.Http
import Melo.Common.Logging
import Melo.Format.Metadata
import Melo.Library.Artist.Service
import Melo.Library.Metadata.Service
import Melo.Lookup.MusicBrainz
import qualified Network.Wreq.Session as Sess
import Network.Wreq.Session (Session)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ArtistService" $ do
    it "" $ do
      initLogging
      sess <- Sess.newSession
      f <- openMetadataFile "/home/chris/Music/01 Web.flac"
      runStdoutLogging $ runHttpSession sess $ runMusicBrainzServiceIO $ runMetadataService $ runArtistServiceIO
        $ replicateM_ 1
        $ identifyArtists f
      pending
