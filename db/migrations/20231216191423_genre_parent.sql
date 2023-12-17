-- migrate:up
create table genre_parent
(
    genre_id      uuid not null
        constraint parent_genre_id_genre_fk
            references genre (id),
    parent_genre uuid not null
        constraint parent_genre_genre_fk
            references genre (id),
    constraint parent_genre_pk
        primary key (genre_id),
    constraint parent_genre_id_parent_u unique (genre_id, parent_genre)
);

-- migrate:down

drop table if exists genre_parent;
