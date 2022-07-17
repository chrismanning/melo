-- migrate:up
create table tag_mapping
(
    name           text  not null
        constraint tag_mapping_pk
            primary key,
    field_mappings jsonb not null
);

create unique index tag_mapping_name_uindex
    on tag_mapping (name);

-- migrate:down
drop table tag_mapping;
