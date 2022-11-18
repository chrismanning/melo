-- migrate:up

drop index if exists artist_name_disambiguation_uindex;

create unique index artist_name_uindex
    on artist (name)
    where musicbrainz_id is null;

create unique index artist_name_musicbrainz_id_uindex
    on artist (name)
    where musicbrainz_id is not null;

-- migrate:down

drop index artist_name_musicbrainz_id_uindex;
drop index artist_name_uindex;
