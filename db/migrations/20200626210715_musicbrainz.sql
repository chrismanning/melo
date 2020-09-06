-- migrate:up

alter table artist
    add musicbrainz_id text;

create unique index artist_musicbrainz_id_uindex
    on artist (musicbrainz_id);

alter table album
    add musicbrainz_id text;

create unique index album_musicbrainz_id_uindex
    on album (musicbrainz_id);

-- migrate:down

