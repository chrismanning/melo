module Melo.Library.Collection.API where

import Control.Foldl qualified as F
import Melo.Common.API
import Melo.Common.Monad
import Melo.Common.Routing
import Melo.Database.Repo as Repo
import Melo.Library.Collection.Aggregate
import Melo.Library.Collection.Repo ()
import Melo.Library.Collection.Types
import Streaming.Prelude qualified as S

newtype GetCollection = GetCollection
  { id :: CollectionRef
  }
  deriving (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON JSONOptions GetCollection

newtype AddCollection = AddCollection
  { newCollection :: NewCollection
  }
  deriving (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON JSONOptions AddCollection

newtype DeleteCollection = DeleteCollection
  { id :: CollectionRef
  }
  deriving (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON JSONOptions DeleteCollection

deleteAllCollections :: AppM IO IO (Vector CollectionRef)
deleteAllCollections = do
  collections <- getAll @CollectionEntity
  S.each collections
    & S.map (.id)
    & S.mapMaybeM deleteCollection
    & F.impurely S.foldM_ F.vectorM

getCollection :: GetCollection -> AppM IO IO (Maybe CollectionEntity)
getCollection GetCollection {id} = getSingle @CollectionEntity id

registerRoutes :: AppM IO IO ()
registerRoutes = do
  registerRoute (RouteKey "getAllCollections") (nullRqJsonRsRoute (getAll @CollectionEntity))
  registerRoute (RouteKey "getCollection") (jsonRqJsonRsRoute getCollection)
  registerRoute (RouteKey "addCollection") (jsonRqJsonRsRoute \AddCollection {newCollection} -> addCollection newCollection)
  registerRoute (RouteKey "deleteAllCollections") (nullRqJsonRsRoute deleteAllCollections)
  registerRoute (RouteKey "deleteCollection") (jsonRqJsonRsRoute \DeleteCollection {id} -> deleteCollection id)
  pure ()
