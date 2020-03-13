module Melo.Http.Client where

import Control.Carrier.Reader
import Network.HTTP.Types
import Network.Wreq
import Network.Wreq.Cache
import Network.Wreq.Session (Session)
import qualified Network.Wreq.Session as Sess

runHttpSession :: ReaderC Session IO a -> IO a
runHttpSession r = do
  s <- Sess.newSession
  runReader s r
