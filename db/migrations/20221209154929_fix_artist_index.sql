-- migrate:up

drop index artist_name_musicbrainz_id_uindex;

create unique index artist_name_musicbrainz_id_uindex
    on artist (name, musicbrainz_id)
    where (musicbrainz_id IS NOT NULL);

-- migrate:down

drop index artist_name_musicbrainz_id_uindex;

create unique index artist_name_musicbrainz_id_uindex
    on artist (name)
    where (musicbrainz_id IS NOT NULL);
