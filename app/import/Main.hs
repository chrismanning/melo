module Main where

import Data.Pool
import Database.PostgreSQL.Simple
import Melo.Common.Logging
import Melo.Library.Filesystem

main :: IO ()
main = do
  initLogging
  let connInfo =
        defaultConnectInfo
          { connectUser = "melo",
            connectPassword = "melo",
            connectDatabase = "melo"
          }
  pool <- createPool (connect connInfo) close 1 20 10
  importDeep pool "/home/chris/Music"
  pure ()
