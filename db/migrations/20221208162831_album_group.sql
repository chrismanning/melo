-- migrate:up

alter table album
    add original_year_released text;

alter table album
    add musicbrainz_group_id text;

alter table album
    add catalogue_number text;

-- migrate:down

alter table album
    drop original_year_released;

alter table album
    drop musicbrainz_group_id;

alter table album
    drop catalogue_number;
