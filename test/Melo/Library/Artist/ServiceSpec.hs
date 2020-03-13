module Melo.Library.Artist.ServiceSpec where

import Control.Carrier.Reader
import Melo.Format.Metadata
import Melo.Library.Artist.Service
import Melo.Library.Metadata.Service
import qualified Network.Wreq.Session as Sess
import Network.Wreq.Session (Session)

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ArtistService" $ do
    it "" $ do
      sess <- Sess.newSession
      f <- openMetadataFile "/home/chris/Music/01 - Son et lumiere.flac"
      runReader sess $ runMetadataService $ runArtistServiceIO $ identifyArtists f
      pending
