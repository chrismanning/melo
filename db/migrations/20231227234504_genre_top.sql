-- migrate:up

alter table genre
    add top_level boolean;
alter table genre
    add short_description text;

alter table genre_parent
    drop constraint if exists parent_genre_pk;

alter table genre_parent
    add constraint parent_genre_pk
        primary key (genre_id, parent_genre);

alter table melo.genre_parent
    drop constraint parent_genre_id_genre_fk;

alter table melo.genre_parent
    add constraint parent_genre_id_genre_fk
        foreign key (genre_id) references melo.genre
            on delete cascade;

alter table melo.genre_parent
    drop constraint parent_genre_genre_fk;

alter table melo.genre_parent
    add constraint parent_genre_genre_fk
        foreign key (parent_genre) references melo.genre
            on delete cascade;

drop index if exists parent_genre_genre_id_index;
drop index if exists parent_genre_parent_genre_index;
drop index if exists genre_top_level_index;

create index parent_genre_genre_id_index on genre_parent (genre_id);
create index parent_genre_parent_genre_index on genre_parent (parent_genre);
create index genre_top_level_index on genre (top_level);

-- migrate:down

alter table genre
    drop short_description;

alter table genre
    drop top_level;

drop index if exists genre_top_level_id_index;

alter table genre_parent
    drop constraint parent_genre_pk;

alter table genre_parent
    drop constraint if exists parent_genre_id_parent_u;
