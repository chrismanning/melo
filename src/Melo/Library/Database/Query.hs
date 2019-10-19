module Melo.Library.Database.Query where

import Control.Lens
import Data.Text (Text)
import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Query.Internal
import Melo.Library.Database.Model

type QL = Q Postgres LibraryDb

type QPgExpr = QExpr Postgres

type QPgAgg = QAgg Postgres

type ReusableQL = ReusableQ Postgres LibraryDb

type WithL = With Postgres LibraryDb

getAllGenresQuery :: QL s (GenreT (QPgExpr s))
getAllGenresQuery = all_ (libraryDb ^. #genre)

getGenresQuery :: Text -> QL s (GenreT (QPgExpr s))
getGenresQuery t = do
  genre <- all_ (libraryDb ^. #genre)
  guard_ (toTsVector Nothing (genre ^. #name) @@ toTsQuery Nothing (val_ (t <> "|" <> t <> ":*")))
  pure genre

getGenreTracksQuery :: [GenreKey] -> QL s (PrimaryKey GenreT (QPgExpr s), TrackT (QPgExpr s))
getGenreTracksQuery pks = do
  let genreIds = fmap (\(GenreKey g) -> val_ g) pks
  (g, t) <-
    manyToMany_
      (libraryDb ^. #track_genre)
      (^. #genre_id)
      (^. #track_id)
      ( filter_
          (\g -> g ^. #id `in_` genreIds)
          (all_ (libraryDb ^. #genre))
      )
      (all_ (libraryDb ^. #track))
  pure (primaryKey g, t)
