module Melo.Library.Genre.Aggregate where

import Melo.Common.Logging
import Melo.Common.Monad
import Melo.Database.Repo.IO
import Melo.Library.Genre.Repo
import Melo.Library.Genre.Types

getTopLevelGenres :: AppM IO IO (Vector Genre)
getTopLevelGenres = do
  $(logInfo) "Getting top-level genres"
  pool <- getConnectionPool
  results <- runSelect pool topLevelGenres
  pure $! from <$!> results
