{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}

module Melo.LibrarySpec where

import Control.Lens
import Data.Aeson as A
import Data.Aeson.Encoding as A
import Data.Attoparsec.Text
import Data.Binary
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.HashMap.Strict as H
import Data.List.NonEmpty
import Data.Pool
import Data.Sequence as Seq
import Data.Text.IO ()
import qualified Data.Text.IO as T
import Data.Vector
import Database.Beam.Backend.SQL.Builder
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax
import Database.Beam.Query
import Database.PostgreSQL.Simple
import Debug.Trace
import GHC.OverloadedLabels
import Haxl.Core (initEnv, runHaxl, stateEmpty, stateSet)
--import qualified Language.GraphQL.AST as AST
import qualified Language.GraphQL.AST.Core as QL
import Language.Haskell.TH
import qualified Melo.API as API
--import qualified Melo.API.Haxl as API

import Melo.Database.Query
import qualified Melo.GraphQL.Resolve as API
import Melo.Library
import qualified Melo.Library.API as API
import Melo.Library.Repo.Haxl as API
import System.IO
import Test.Hspec
import Text.RawString.QQ (r)

-- defineByDocumentFile "src/Melo/Library/API copy.gql"
--   [gql|
--     query GetGenres {
--       library {
--         genres {
--           slug
--           name
--           tracks {
--             title
--           }
--         }
--       }
--     }
--   |]

main :: IO ()
main = hspec spec

-- dumpPgSqlSelect :: Projectible Postgres res
--               => Q Postgres db QBaseScope res -> IO ()
-- dumpPgSqlSelect q = do
--   let (SqlSelect (PgSelectSyntax e)) = select q
--   -- conn <- connectPostgreSQL "host='localhost' dbname='postgres' user='postgres' password='postgres'"
--   putStrLn $ show $ pgRenderSyntaxScript e

spec :: Spec
spec = do
  describe "Library" $ do
    it "" pending
    -- it "generates query" $ do
    --   dumpPgSqlSelect $
    --     (albumArtistAliasRelationship (all_ $ meloDb ^. #artistAliases) (all_ $ meloDb ^. #albums))
    it "finds genre + track" $ do
      let connInfo =
            defaultConnectInfo
              { connectUser = "melo",
                connectPassword = "melo",
                connectDatabase = "melo"
              }
      conn <- connect connInfo
      --      connPool <- createPool (connect connInfo) close 1 (fromInteger 10) 10
      -- expr <- runQ (defineByDocumentFile "src/Melo/Library/API copy.gql"
      --   [gql|
      --     query GetGenres {
      --       library {
      --         genres {
      --           slug
      --           name
      --         }
      --       }
      --     }
      --   |])
      -- putStrLn $ pprint expr
      -- gs :: Either String GetGenres <- fetch (QLAPI.apiIO h) GetGenresArgs {}
      -- putStrLn $ show gs
      let x :: API.Haxl A.Encoding =
            API.resolveFieldValues @API.Haxl @API.Library
              (API.LibraryCtx)
              -- [ Field
              -- Nothing
              -- "library"
              -- []
              [ QL.Field
                  Nothing
                  "genres"
                  (QL.Arguments $ H.singleton "name" (QL.String "prog"))
                  $ Seq.fromList
                    [ QL.SelectionField $ QL.Field Nothing "slug" (QL.Arguments H.empty) Seq.empty,
                      QL.SelectionField $ QL.Field Nothing "name" (QL.Arguments H.empty) Seq.empty,
                      QL.SelectionField $
                        QL.Field
                          Nothing
                          "tracks"
                          (QL.Arguments H.empty)
                          $ Seq.fromList
                            [ QL.SelectionField $ QL.Field Nothing "slug" (QL.Arguments H.empty) Seq.empty,
                              QL.SelectionField $ QL.Field Nothing "title" (QL.Arguments H.empty) Seq.empty
                            ]
                    ]
              ]
      -- ]
      let stateStore = stateSet API.GenreState {conn} stateEmpty
      e <- initEnv stateStore ()
      rs <- runHaxl e x
      L8.putStrLn $ A.encodingToLazyByteString rs
      pending

--   dumpPgSqlSelect artistAlbums
-- it "reads entire library" $ do
--   let queryTxt = [r|{
--     library {
--       genres(where: $genreWhere, sort: $genreSort) {
--         id
--         name
--         artists(where: $artistWhere) {
--           id
--           name
--           albums(where: $albumWhere) {
--             id
--             title
--             yearReleased
--             tracks(where: $trackWhere) {
--               id
--               title
--               trackNumber
--             }
--           }
--         }
--       }
--     }
--   }|]
--   let (Right query) = parseOnly queryDocument queryTxt
--   traceM $ "QueryDocument: " <> show query
--   traceM $ "Resolved Query: " <> show (resolveQuery query)
--   traceM $ "Transformed: " <> show (transformQuery =<< resolveQuery query)
--   pending
-- it "reads genres" $ do
--   let queryTxt = [r|{
--     library {
--       genres {
--         id
--         name
--         description
--       }
--     }
--   }|]
--   let (Right query) = parseOnly queryDocument queryTxt
--   traceM $ "QueryDocument: " <> show query
--   traceM $ "Resolved Query: " <> show (resolveQuery query)
--   traceM $ "Transformed: " <> show (transformQuery =<< resolveQuery query)
--   pending

-- defineByDocumentFile "src/Melo/Library/API copy.gql"
--   [gql|
--     query GetGenreTracks {
--       library {
--         genres {
--           slug
--           name
--           tracks {
--             slug
--             title
--           }
--         }
--       }
--     }
--   |]
