-- migrate:up

alter table source
    add collection_id uuid not null;

alter table source
    add constraint source_collection_id_fk
        foreign key (collection_id) references collection
            on delete cascade;

-- migrate:down

alter table source
    drop constraint source_collection_id_fk;

alter table source
    drop collection_id;
