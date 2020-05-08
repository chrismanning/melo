-- migrate:up

create schema melo;

set search_path = melo;

create extension if not exists pgcrypto;
create extension if not exists "uuid-ossp";

alter database melo set intervalstyle = 'iso_8601';

create type intervalrange as range
(
    subtype = interval
);

create table genre
(
    id          uuid        not null primary key default uuid_generate_v4(),
    name        text unique not null,
    description text
);

create table artist
(
    id             uuid not null primary key default uuid_generate_v4(),
    name           text not null,
    disambiguation text,
    short_bio      text,
    bio            text,
    country        text
);

create unique index artist_name_disambiguation_uindex on artist (name, disambiguation);

create table artist_name
(
    id        uuid not null primary key default uuid_generate_v4(),
    artist_id uuid not null references artist,
    name      text not null
);

create table related_artist
(
    artist_id         uuid not null references artist,
    related_artist_id uuid not null references artist,
    primary key (artist_id, related_artist_id)
);

create table album
(
    id            uuid not null primary key default uuid_generate_v4(),
    title         text not null,
    comment       text,
    year_released text,
    length        interval
);

create table album_artist_name
(
    album_id       uuid not null references album on delete cascade,
    artist_name_id uuid not null references artist_name on delete cascade,
    primary key (album_id, artist_name_id)
);

create table source
(
    id              uuid  not null unique primary key default uuid_generate_v4(),
    kind            text  not null,
    metadata_format text  not null,
    metadata        jsonb not null,
    source_uri      text  not null,
    idx             int8  not null,
    time_range      intervalrange,
    sample_range    int8range,
    scanned         timestamp,
    unique (source_uri, idx)
);

create table track
(
    id           uuid     not null unique primary key default uuid_generate_v4(),
    title        text     not null,
    track_number int      not null,
    disc_number  int,
    comment      text,
    album_id     uuid     not null references album,
    length       interval not null,
    source_id    uuid     not null
        references source
            on update cascade on delete cascade
);

create table track_artist_name
(
    track_id       uuid not null references track on delete cascade,
    artist_name_id uuid not null references artist_name on delete cascade,
    primary key (track_id, artist_name_id)
);

create table artist_genre
(
    artist_id uuid not null references artist on delete cascade,
    genre_id  uuid not null references genre on delete cascade,
    primary key (artist_id, genre_id)
);

create table album_genre
(
    album_id uuid not null references album on delete cascade,
    genre_id uuid not null references genre on delete cascade,
    primary key (album_id, genre_id)
);

create table track_genre
(
    track_id uuid not null references track on delete cascade,
    genre_id uuid not null references genre on delete cascade,
    primary key (track_id, genre_id)
);

create table artist_stage
(
    id             uuid not null primary key default uuid_generate_v4(),
    name           text,
    disambiguation text,
    short_bio      text,
    bio            text,
    country        text,
    ref_artist_id  uuid references artist,
    ref_album_id   uuid references album,
    ref_track_id   uuid references track
);

create table album_stage
(
    id            uuid not null primary key default uuid_generate_v4(),
    title         text,
    comment       text,
    year_released text,
    length        interval,
    ref_artist_id uuid references artist,
    ref_album_id  uuid references album,
    ref_track_id  uuid references track
);

create table track_stage
(
    id            uuid not null unique primary key default uuid_generate_v4(),
    title         text,
    track_number  int,
    disc_number   int,
    comment       text,
    album_id      uuid references album,
    length        interval,
    source_id     uuid
        references source
            on update cascade on delete cascade,
    ref_artist_id uuid references artist,
    ref_album_id  uuid references album,
    ref_track_id  uuid references track
);

-- migrate:down

