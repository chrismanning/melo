-- migrate:up

alter table artist_name
    drop constraint artist_name_artist_id_fkey;

alter table artist_name
    add foreign key (artist_id) references artist
        on delete cascade;

-- migrate:down

alter table artist_name
    drop constraint artist_name_artist_id_fkey;

alter table artist_name
    add foreign key (artist_id) references artist;
