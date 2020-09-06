module Main where

import Control.Monad.IO.Class
import Data.Pool
import Database.PostgreSQL.Simple
import Melo.API
import Melo.Common.Logging
import Web.Scotty

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
  scotty 5000 (api pool)
