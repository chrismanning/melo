-- migrate:up

create index source_collection_id_index
    on source (collection_id);

-- migrate:down

drop index source_collection_id_index;
