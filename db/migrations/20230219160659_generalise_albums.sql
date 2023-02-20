-- migrate:up

alter table album
    rename to release;

alter table release
    rename constraint album_pkey to release_pkey;

alter table release
    add column kind text not null default 'album';

alter index album_musicbrainz_id_uindex
    rename to release_musicbrainz_id_uindex;

alter index album_musicbrainz_group_id_uindex
    rename to release_musicbrainz_group_id_uindex;

alter index album_title_year_released_uindex
    rename to release_title_year_released_uindex;

alter table track
    rename column album_id to release_id;

alter table track
    rename constraint track_album_id_fkey to track_release_id_fkey;

alter index track_track_number_disc_number_album_id_uindex rename to track_track_number_disc_number_release_id_uindex;

alter table album_artist_name
    rename to release_artist_name;

alter table release_artist_name
    rename column album_id to release_id;

alter table release_artist_name
    rename constraint album_artist_name_album_id_fkey to release_artist_name_release_id_fkey;

alter table release_artist_name
    rename constraint album_artist_name_artist_name_id_fkey to release_artist_name_artist_name_id_fkey;

alter table release_artist_name
    rename constraint album_artist_name_pkey to release_artist_name_pkey;

alter table album_genre
    rename to release_genre;

alter table release_genre
    rename column album_id to release_id;

alter table release_genre
    rename constraint album_genre_pkey to release_genre_pkey;

alter table release_genre
    rename constraint album_genre_album_id_fkey to release_genre_release_id_fkey;

alter table release_genre
    rename constraint album_genre_genre_id_fkey to release_genre_genre_id_fkey;

-- migrate:down

alter table release_genre
    rename to album_genre;

alter table album_genre
    rename column release_id to album_id;

alter table album_genre
    rename constraint release_genre_pkey to album_genre_pkey;

alter table album_genre
    rename constraint release_genre_release_id_fkey to album_genre_album_id_fkey;

alter table album_genre
    rename constraint release_genre_genre_id_fkey to album_genre_genre_id_fkey;

alter table release_artist_name
    rename to album_artist_name;

alter table album_artist_name
    rename column release_id to album_id;

alter table album_artist_name
    rename constraint release_artist_name_release_id_fkey to album_artist_name_album_id_fkey;

alter table album_artist_name
    rename constraint release_artist_name_artist_name_id_fkey to album_artist_name_artist_name_id_fkey;

alter table album_artist_name
    rename constraint release_artist_name_pkey to album_artist_name_pkey;

alter table release
    rename to album;

alter table album
    rename constraint release_pkey to album_pkey;

alter index release_musicbrainz_id_uindex
    rename to album_musicbrainz_id_uindex;

alter index release_musicbrainz_group_id_uindex
    rename to album_musicbrainz_group_id_uindex;

alter index release_title_year_released_uindex
    rename to album_title_year_released_uindex;

alter table track
    rename column release_id to album_id;

alter table track
    rename constraint track_release_id_fkey to track_album_id_fkey;

alter index track_track_number_disc_number_release_id_uindex rename to track_track_number_disc_number_album_id_uindex;
