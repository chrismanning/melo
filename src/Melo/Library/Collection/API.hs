module Melo.Library.Collection.API where

import Control.Foldl qualified as F
import Data.Pool
import Hasql.Session qualified as Hasql
import Melo.Common.API
import Melo.Common.Exception
import Melo.Common.Monad
import Melo.Common.Routing
import Melo.Database.Repo as Repo
import Melo.Database.Repo.IO
import Melo.Library.Collection.Aggregate
import Melo.Library.Collection.Repo ()
import Melo.Library.Collection.Types
import Melo.Library.Source.Repo
import Melo.Library.Source.Types
import Rel8 qualified
import Streaming.Prelude qualified as S

newtype CollectionRefWrapper = CollectionRefWrapper
  { id :: CollectionRef
  }
  deriving (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON JSONOptions CollectionRefWrapper

deleteAllCollections :: AppM IO IO (Vector CollectionRef)
deleteAllCollections = do
  collections <- getAll @CollectionEntity
  S.each collections
    & S.map (.id)
    & S.mapMaybeM deleteCollection
    & F.impurely S.foldM_ F.vectorM

data CollectionStatistics = CollectionStatistics
  { totalSources :: Integer
  }
  deriving (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON JSONOptions CollectionStatistics

getCollectionStatistics :: CollectionRefWrapper -> AppM IO IO CollectionStatistics
getCollectionStatistics req = do
  pool <- getConnectionPool
  let session = Hasql.statement () $ Rel8.run1 $ Rel8.select query
  totalSources <- fromIntegral <$> liftIO do
    withResource pool $ \conn -> Hasql.run session conn >>= throwOnLeft
  pure
    CollectionStatistics
      { totalSources
      }
    where
      query = Rel8.countRows do
        source <- Rel8.each sourceSchema
        Rel8.where_ $ source.collection_id Rel8.==. Rel8.lit req.id
        pure source

scanCollection :: CollectionRefWrapper -> AppM IO IO ()
scanCollection req = void $ fork (rescanCollection req.id)

data UpdateCollection = UpdateCollection
  { id :: CollectionRef
  , updates :: CollectionUpdates
  }
  deriving (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON JSONOptions UpdateCollection

registerRoutes :: AppM IO IO ()
registerRoutes = do
  registerRoute (RouteKey "addCollection") (jsonRqJsonRsRoute addCollection)
  registerRoute (RouteKey "deleteAllCollections") (nullRqJsonRsRoute deleteAllCollections)
  registerRoute (RouteKey "deleteCollection") (jsonRqFnfRoute \CollectionRefWrapper {id} -> void $ deleteCollection id)
  registerRoute (RouteKey "getAllCollections") (nullRqJsonRsRoute (getAll @CollectionEntity))
  registerRoute (RouteKey "getCollection") (jsonRqJsonRsRoute \CollectionRefWrapper {id} -> getSingle @CollectionEntity id)
  registerRoute (RouteKey "getCollectionStatistics") (jsonRqJsonRsRoute getCollectionStatistics)
  registerRoute (RouteKey "scanCollection") (jsonRqFnfRoute scanCollection)
  registerRoute (RouteKey "updateCollection") (jsonRqFnfRoute \UpdateCollection {id, updates} -> updateCollection id updates)
  pure ()
