-- migrate:up

create unique index album_musicbrainz_group_id_uindex
    on album (musicbrainz_group_id)
    where musicbrainz_id is null;

create unique index album_title_year_released_uindex
    on album (title, year_released)
    where musicbrainz_id is null and musicbrainz_group_id is null;

-- migrate:down

drop index album_musicbrainz_group_id_uindex;
drop index album_title_year_released_uindex;
