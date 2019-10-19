create extension if not exists pgcrypto;
create extension if not exists "uuid-ossp";

create table genre (
    id uuid not null primary key default uuid_generate_v4(),
    name text not null,
    description text
);

create table artist (
    id uuid not null primary key default uuid_generate_v4(),
    name text not null,
    short_bio text,
    bio text,
    country text
);

create table artist_alias (
    id uuid not null primary key default uuid_generate_v4(),
    artist_id uuid not null references artist,
    alias text not null
);

create table related_artist (
    artist_id uuid not null references artist,
    related_artist_id uuid not null references artist,
    primary key (artist_id, related_artist_id)
);

create table album (
    id uuid not null primary key default uuid_generate_v4(),
    title text not null,
    comment text,
    year_released text,
    length interval
);

create table album_artist_alias (
    album_id uuid not null references album on delete cascade,
    artist_alias_id uuid not null references artist_alias on delete cascade,
    primary key (album_id, artist_alias_id)
);

create table audio_source (
    id uuid not null primary key default uuid_generate_v4(),
    kind text not null,
    source text not null,
    format text not null,
    sample_rate integer not null,
    bits_per_sample smallint,
    channels smallint not null,
    total_samples bigint
);

create table metadata_source (
    id uuid not null primary key default uuid_generate_v4(),
    kind text not null,
    source text not null,
    idx text
);

create table track (
    id uuid not null primary key default uuid_generate_v4(),
    title text not null,
    track_number int,
    comment text,
    album_id uuid references album,
    length interval not null,
    audio_source_id uuid not null
        references audio_source
            on update cascade on delete cascade,
    metadata_source_id uuid not null
        references metadata_source
            on update cascade on delete cascade,
    "offset" interval default '00:00:00'::interval not null
);

create table track_artist_alias (
    track_id uuid not null references track on delete cascade,
    artist_alias_id uuid not null references artist_alias on delete cascade,
    primary key (track_id, artist_alias_id)
);

create table artist_genre (
    artist_id uuid not null references artist on delete cascade,
    genre_id uuid not null references genre on delete cascade,
    primary key (artist_id, genre_id)
);

create table album_genre (
    album_id uuid not null references album on delete cascade,
    genre_id uuid not null references genre on delete cascade,
    primary key (album_id, genre_id)
);

create table track_genre (
    track_id uuid not null references track on delete cascade,
    genre_id uuid not null references genre on delete cascade,
    primary key (track_id, genre_id)
);
