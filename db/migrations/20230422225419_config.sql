-- migrate:up

create table if not exists config
(
    key   text  not null
        constraint config_pk
            primary key,
    value jsonb not null
);

-- migrate:down

drop table if exists config;
