{-# LANGUAGE UndecidableInstances #-}

module Melo.Metadata.Mapping.Repo where

import Melo.Common.Monad
import Melo.Database.Repo
import Melo.Database.Repo.IO
import Melo.Metadata.Mapping.Types
import Rel8
  ( Name,
    TableSchema (..),
    Upsert (..),
    (==.),
  )

type TagMappingRepository = Repository TagMappingEntity

tagMappingSchema :: TableSchema (TagMappingTable Name)
tagMappingSchema =
  TableSchema
    { name = "tag_mapping",
      schema = Nothing,
      columns =
        TagMappingTable
          { name = "name",
            field_mappings = "field_mappings"
          }
    }

initTagMappingRepo :: AppDataReader m => m ()
initTagMappingRepo = putAppData
  RepositoryHandle
    { tbl = tagMappingSchema,
      pk = (.name),
      upsert =
        Just
          Upsert
            { index = (.name),
              set = const,
              updateWhere = \new old -> new.name ==. old.name
            }
    }
