module Main where

import Database.PostgreSQL.Simple
import Melo.Library.Filesystem

main :: IO ()
main = do
  let connInfo =
        defaultConnectInfo
          { connectUser = "melo",
            connectPassword = "melo",
            connectDatabase = "melo"
          }
  conn <- connect connInfo
--  scanPath conn "/home/chris/Music"
  pure ()
