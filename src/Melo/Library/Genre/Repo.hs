module Melo.Library.Genre.Repo where

import Data.Functor.Contravariant
import Melo.Common.Monad
import Melo.Database.Repo as Repo
import Melo.Database.Repo.IO
import Melo.Library.Genre.Types
import Rel8
  ( Expr,
    ListTable,
    Name,
    Query,
    TableSchema (..),
    Upsert (..),
    asc,
    each,
    in_,
    lit,
    many,
    manyExpr,
    orderBy,
    some,
    where_,
    (&&.),
    (==.),
  )
import Rel8 qualified

type GenreRepository = Repository GenreEntity

topLevelGenres :: Query (TopLevelGenre Expr)
topLevelGenres = do
  --  orderBy ((.name) >$< asc) $ each topLevelGenreSchema
  topLevelGenre <- each genreSchema
  where_ topLevelGenre.top_level
  parentRefs <- many do
    genre <- each genreSchema
    parent <- each genreParentSchema
    where_ $ parent.genre_id ==. topLevelGenre.id &&. genre.id ==. parent.parent_genre
    pure (parent.parent_genre, genre.name)
  childRefs <- many do
    genre <- each genreSchema
    parent <- each genreParentSchema
    where_ $ parent.parent_genre ==. topLevelGenre.id &&. genre.id ==. parent.genre_id
    pure (parent.genre_id, genre.name)
  pure (topLevelGenre, parentRefs, childRefs)

genreParents :: GenreRef -> Query (GenreTable Expr)
genreParents ref = do
  parents <- each genreParentSchema
  genres <- each genreSchema
  where_ $ parents.genre_id ==. lit ref &&. genres.id ==. parents.parent_genre
  pure genres

genreChildren :: GenreRef -> Query (GenreTable Expr)
genreChildren ref = do
  parents <- each genreParentSchema
  genres <- each genreSchema
  where_ $ parents.parent_genre ==. lit ref &&. genres.id ==. parents.genre_id
  pure genres

genreSchema :: TableSchema (GenreTable Name)
genreSchema =
  TableSchema
    { name = "genre",
      columns =
        GenreTable
          { id = "id",
            name = "name",
            description = "description",
            top_level = "top_level"
          }
    }

genreParentSchema :: TableSchema (GenreParentTable Name)
genreParentSchema =
  TableSchema
    { name = "genre_parent",
      columns =
        GenreParentTable
          { genre_id = "genre_id",
            parent_genre = "parent_genre"
          }
    }

topLevelGenreSchema :: TableSchema (TopLevelGenreTable Name)
topLevelGenreSchema =
  TableSchema
    { name = "genre_top_level_tree",
      columns =
        TopLevelGenreTable
          { id = "id",
            name = "name",
            description = "description",
            parents = "parents",
            children = "children"
          }
    }

initGenreRepo :: (AppDataReader m) => m ()
initGenreRepo =
  putAppData
    RepositoryHandle
      { tbl = genreSchema,
        pk = \e -> e.id,
        upsert =
          Just
            Upsert
              { index = \c -> c.name,
                predicate = Nothing,
                set = const,
                updateWhere = \new old -> new.id ==. old.id
              }
      }
