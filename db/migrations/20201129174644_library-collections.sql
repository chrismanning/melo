-- migrate:up

create table collection
(
    id uuid default uuid_generate_v4() not null,
    root_uri text not null,
    name text not null,
    watch bool not null,
    kind text not null
);

create unique index collection_id_uindex
    on collection (id);

create unique index collection_name_uindex
    on collection (name);

create unique index collection_root_uri_uindex
    on collection (root_uri);

alter table collection
    add constraint collection_pk
        primary key (id);

-- migrate:down

drop table collection;
