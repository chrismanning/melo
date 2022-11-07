-- migrate:up

drop table if exists album_stage;

drop table if exists artist_stage;

drop table if exists track_stage;

-- migrate:down

