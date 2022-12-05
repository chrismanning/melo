-- migrate:up

create unique index artist_name_artist_id_name_uindex
    on artist_name (artist_id, name);

-- migrate:down

drop index if exists artist_name_artist_id_name_uindex;
