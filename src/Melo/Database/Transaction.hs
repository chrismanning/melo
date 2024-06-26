{-# LANGUAGE UndecidableInstances #-}

module Melo.Database.Transaction where

import Control.Monad.Reader
import Data.Pool as P
import Hasql.Connection
import Hasql.Decoders
import Hasql.Encoders
import Hasql.Session
import Hasql.Statement
import Melo.Common.Exception as E
import Melo.Common.Logging

withTransaction :: (MonadMask m, MonadIO m) => Pool Connection -> (Connection -> t -> m a) -> t -> m a
withTransaction pool runner t =
  E.mask $ \restore -> do
    (conn, localPool) <- liftIO $ takeResource pool
    liftIO $ run begin conn >>= either throwIO pure
    $(logDebugIO) "transaction begun"
    r <- restore (runner conn t) `E.onException` cleanUp conn localPool
    liftIO $ run commit conn >>= either throwIO pure
    $(logDebugIO) "transaction committed"
    liftIO $ putResource localPool conn
    return r
  where
    begin = statement () $ Statement "BEGIN" noParams noResult False
    commit = statement () $ Statement "COMMIT" noParams noResult False
    rollback = statement () $ Statement "ROLLBACK" noParams noResult False
    cleanUp conn localPool =
      liftIO $ do
        run rollback conn >>= either throwIO pure
        $(logWarnIO) "transaction rolled back"
        putResource localPool conn
