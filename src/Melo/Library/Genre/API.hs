module Melo.Library.Genre.API where

import Melo.Common.API
import Melo.Common.Monad
import Melo.Common.Routing
import Melo.Library.Genre.Aggregate

registerRoutes :: AppM IO IO ()
registerRoutes = do
  registerRoute (RouteKey "getTopLevelGenres") (nullRqJsonRsRoute getTopLevelGenres)
  pure ()
