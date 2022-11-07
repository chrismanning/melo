-- migrate:up

alter table track
    add musicbrainz_id text;

create unique index track_musicbrainz_id_uindex
    on track (musicbrainz_id);

create unique index track_source_id_uindex
    on track (source_id);

create unique index track_track_number_disc_number_album_id_uindex
    on track (track_number, disc_number, album_id);


-- migrate:down

