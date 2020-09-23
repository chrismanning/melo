module Melo.Library.Artist.ServiceSpec where

import Control.Carrier.Empty.Church
import Control.Carrier.Reader
import Control.Lens
import Control.Monad
import Data.HashMap.Strict as H
import Melo.Common.Http
import Melo.Common.Logging
import Melo.Format.Metadata
import Melo.Library.Artist.Service
import Melo.Library.Service
import Melo.Lookup.MusicBrainz
import Network.Wreq.Session (Session)
import qualified Network.Wreq.Session as Sess
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ArtistService" $ do
    it "" $ do
      --      initLogging
      --      sess <- Sess.newSession
      --      f <- openMetadataFile "/home/chris/Music/01 Web.flac"
      --      runMetadataService (chooseMetadata (H.elems (f ^. #metadata))) >>= \case
      --        Nothing -> expectationFailure ""
      --        Just m -> do
      --          evalEmpty $ runStdoutLogging $ runMusicBrainzServiceIO sess $ runMetadataService $ runArtistServiceIO
      --            $ replicateM_ 1
      --            $ identifyArtists m
      pending
