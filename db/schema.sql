SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: melo; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA melo;


--
-- Name: pgcrypto; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS pgcrypto WITH SCHEMA melo;


--
-- Name: EXTENSION pgcrypto; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION pgcrypto IS 'cryptographic functions';


--
-- Name: uuid-ossp; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS "uuid-ossp" WITH SCHEMA melo;


--
-- Name: EXTENSION "uuid-ossp"; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION "uuid-ossp" IS 'generate universally unique identifiers (UUIDs)';


--
-- Name: intervalrange; Type: TYPE; Schema: melo; Owner: -
--

CREATE TYPE melo.intervalrange AS RANGE (
    subtype = interval
);


SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: album; Type: TABLE; Schema: melo; Owner: -
--

CREATE TABLE melo.album (
    id uuid DEFAULT melo.uuid_generate_v4() NOT NULL,
    title text NOT NULL,
    comment text,
    year_released text,
    length interval,
    musicbrainz_id text
);


--
-- Name: album_artist_name; Type: TABLE; Schema: melo; Owner: -
--

CREATE TABLE melo.album_artist_name (
    album_id uuid NOT NULL,
    artist_name_id uuid NOT NULL
);


--
-- Name: album_genre; Type: TABLE; Schema: melo; Owner: -
--

CREATE TABLE melo.album_genre (
    album_id uuid NOT NULL,
    genre_id uuid NOT NULL
);


--
-- Name: album_stage; Type: TABLE; Schema: melo; Owner: -
--

CREATE TABLE melo.album_stage (
    id uuid DEFAULT melo.uuid_generate_v4() NOT NULL,
    title text,
    comment text,
    year_released text,
    length interval,
    ref_artist_id uuid,
    ref_album_id uuid,
    ref_track_id uuid
);


--
-- Name: artist; Type: TABLE; Schema: melo; Owner: -
--

CREATE TABLE melo.artist (
    id uuid DEFAULT melo.uuid_generate_v4() NOT NULL,
    name text NOT NULL,
    disambiguation text,
    short_bio text,
    bio text,
    country text,
    musicbrainz_id text
);


--
-- Name: artist_genre; Type: TABLE; Schema: melo; Owner: -
--

CREATE TABLE melo.artist_genre (
    artist_id uuid NOT NULL,
    genre_id uuid NOT NULL
);


--
-- Name: artist_name; Type: TABLE; Schema: melo; Owner: -
--

CREATE TABLE melo.artist_name (
    id uuid DEFAULT melo.uuid_generate_v4() NOT NULL,
    artist_id uuid NOT NULL,
    name text NOT NULL
);


--
-- Name: artist_stage; Type: TABLE; Schema: melo; Owner: -
--

CREATE TABLE melo.artist_stage (
    id uuid DEFAULT melo.uuid_generate_v4() NOT NULL,
    name text,
    disambiguation text,
    short_bio text,
    bio text,
    country text,
    ref_artist_id uuid,
    ref_album_id uuid,
    ref_track_id uuid
);


--
-- Name: genre; Type: TABLE; Schema: melo; Owner: -
--

CREATE TABLE melo.genre (
    id uuid DEFAULT melo.uuid_generate_v4() NOT NULL,
    name text NOT NULL,
    description text
);


--
-- Name: related_artist; Type: TABLE; Schema: melo; Owner: -
--

CREATE TABLE melo.related_artist (
    artist_id uuid NOT NULL,
    related_artist_id uuid NOT NULL
);


--
-- Name: source; Type: TABLE; Schema: melo; Owner: -
--

CREATE TABLE melo.source (
    id uuid DEFAULT melo.uuid_generate_v4() NOT NULL,
    kind text NOT NULL,
    metadata_format text NOT NULL,
    metadata jsonb NOT NULL,
    source_uri text NOT NULL,
    idx bigint NOT NULL,
    time_range melo.intervalrange,
    sample_range int8range,
    scanned timestamp without time zone
);


--
-- Name: track; Type: TABLE; Schema: melo; Owner: -
--

CREATE TABLE melo.track (
    id uuid DEFAULT melo.uuid_generate_v4() NOT NULL,
    title text NOT NULL,
    track_number integer NOT NULL,
    disc_number integer,
    comment text,
    album_id uuid NOT NULL,
    length interval NOT NULL,
    source_id uuid NOT NULL
);


--
-- Name: track_artist_name; Type: TABLE; Schema: melo; Owner: -
--

CREATE TABLE melo.track_artist_name (
    track_id uuid NOT NULL,
    artist_name_id uuid NOT NULL
);


--
-- Name: track_genre; Type: TABLE; Schema: melo; Owner: -
--

CREATE TABLE melo.track_genre (
    track_id uuid NOT NULL,
    genre_id uuid NOT NULL
);


--
-- Name: track_stage; Type: TABLE; Schema: melo; Owner: -
--

CREATE TABLE melo.track_stage (
    id uuid DEFAULT melo.uuid_generate_v4() NOT NULL,
    title text,
    track_number integer,
    disc_number integer,
    comment text,
    album_id uuid,
    length interval,
    source_id uuid,
    ref_artist_id uuid,
    ref_album_id uuid,
    ref_track_id uuid
);


--
-- Name: schema_migrations; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.schema_migrations (
    version character varying(255) NOT NULL
);


--
-- Name: album_artist_name album_artist_name_pkey; Type: CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.album_artist_name
    ADD CONSTRAINT album_artist_name_pkey PRIMARY KEY (album_id, artist_name_id);


--
-- Name: album_genre album_genre_pkey; Type: CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.album_genre
    ADD CONSTRAINT album_genre_pkey PRIMARY KEY (album_id, genre_id);


--
-- Name: album album_pkey; Type: CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.album
    ADD CONSTRAINT album_pkey PRIMARY KEY (id);


--
-- Name: album_stage album_stage_pkey; Type: CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.album_stage
    ADD CONSTRAINT album_stage_pkey PRIMARY KEY (id);


--
-- Name: artist_genre artist_genre_pkey; Type: CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.artist_genre
    ADD CONSTRAINT artist_genre_pkey PRIMARY KEY (artist_id, genre_id);


--
-- Name: artist_name artist_name_pkey; Type: CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.artist_name
    ADD CONSTRAINT artist_name_pkey PRIMARY KEY (id);


--
-- Name: artist artist_pkey; Type: CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.artist
    ADD CONSTRAINT artist_pkey PRIMARY KEY (id);


--
-- Name: artist_stage artist_stage_pkey; Type: CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.artist_stage
    ADD CONSTRAINT artist_stage_pkey PRIMARY KEY (id);


--
-- Name: genre genre_name_key; Type: CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.genre
    ADD CONSTRAINT genre_name_key UNIQUE (name);


--
-- Name: genre genre_pkey; Type: CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.genre
    ADD CONSTRAINT genre_pkey PRIMARY KEY (id);


--
-- Name: related_artist related_artist_pkey; Type: CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.related_artist
    ADD CONSTRAINT related_artist_pkey PRIMARY KEY (artist_id, related_artist_id);


--
-- Name: source source_pkey; Type: CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.source
    ADD CONSTRAINT source_pkey PRIMARY KEY (id);


--
-- Name: source source_source_uri_idx_key; Type: CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.source
    ADD CONSTRAINT source_source_uri_idx_key UNIQUE (source_uri, idx);


--
-- Name: track_artist_name track_artist_name_pkey; Type: CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.track_artist_name
    ADD CONSTRAINT track_artist_name_pkey PRIMARY KEY (track_id, artist_name_id);


--
-- Name: track_genre track_genre_pkey; Type: CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.track_genre
    ADD CONSTRAINT track_genre_pkey PRIMARY KEY (track_id, genre_id);


--
-- Name: track track_pkey; Type: CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.track
    ADD CONSTRAINT track_pkey PRIMARY KEY (id);


--
-- Name: track_stage track_stage_pkey; Type: CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.track_stage
    ADD CONSTRAINT track_stage_pkey PRIMARY KEY (id);


--
-- Name: schema_migrations schema_migrations_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.schema_migrations
    ADD CONSTRAINT schema_migrations_pkey PRIMARY KEY (version);


--
-- Name: album_musicbrainz_id_uindex; Type: INDEX; Schema: melo; Owner: -
--

CREATE UNIQUE INDEX album_musicbrainz_id_uindex ON melo.album USING btree (musicbrainz_id);


--
-- Name: artist_musicbrainz_id_uindex; Type: INDEX; Schema: melo; Owner: -
--

CREATE UNIQUE INDEX artist_musicbrainz_id_uindex ON melo.artist USING btree (musicbrainz_id);


--
-- Name: artist_name_disambiguation_uindex; Type: INDEX; Schema: melo; Owner: -
--

CREATE UNIQUE INDEX artist_name_disambiguation_uindex ON melo.artist USING btree (name, disambiguation);


--
-- Name: album_artist_name album_artist_name_album_id_fkey; Type: FK CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.album_artist_name
    ADD CONSTRAINT album_artist_name_album_id_fkey FOREIGN KEY (album_id) REFERENCES melo.album(id) ON DELETE CASCADE;


--
-- Name: album_artist_name album_artist_name_artist_name_id_fkey; Type: FK CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.album_artist_name
    ADD CONSTRAINT album_artist_name_artist_name_id_fkey FOREIGN KEY (artist_name_id) REFERENCES melo.artist_name(id) ON DELETE CASCADE;


--
-- Name: album_genre album_genre_album_id_fkey; Type: FK CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.album_genre
    ADD CONSTRAINT album_genre_album_id_fkey FOREIGN KEY (album_id) REFERENCES melo.album(id) ON DELETE CASCADE;


--
-- Name: album_genre album_genre_genre_id_fkey; Type: FK CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.album_genre
    ADD CONSTRAINT album_genre_genre_id_fkey FOREIGN KEY (genre_id) REFERENCES melo.genre(id) ON DELETE CASCADE;


--
-- Name: album_stage album_stage_ref_album_id_fkey; Type: FK CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.album_stage
    ADD CONSTRAINT album_stage_ref_album_id_fkey FOREIGN KEY (ref_album_id) REFERENCES melo.album(id);


--
-- Name: album_stage album_stage_ref_artist_id_fkey; Type: FK CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.album_stage
    ADD CONSTRAINT album_stage_ref_artist_id_fkey FOREIGN KEY (ref_artist_id) REFERENCES melo.artist(id);


--
-- Name: album_stage album_stage_ref_track_id_fkey; Type: FK CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.album_stage
    ADD CONSTRAINT album_stage_ref_track_id_fkey FOREIGN KEY (ref_track_id) REFERENCES melo.track(id);


--
-- Name: artist_genre artist_genre_artist_id_fkey; Type: FK CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.artist_genre
    ADD CONSTRAINT artist_genre_artist_id_fkey FOREIGN KEY (artist_id) REFERENCES melo.artist(id) ON DELETE CASCADE;


--
-- Name: artist_genre artist_genre_genre_id_fkey; Type: FK CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.artist_genre
    ADD CONSTRAINT artist_genre_genre_id_fkey FOREIGN KEY (genre_id) REFERENCES melo.genre(id) ON DELETE CASCADE;


--
-- Name: artist_name artist_name_artist_id_fkey; Type: FK CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.artist_name
    ADD CONSTRAINT artist_name_artist_id_fkey FOREIGN KEY (artist_id) REFERENCES melo.artist(id);


--
-- Name: artist_stage artist_stage_ref_album_id_fkey; Type: FK CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.artist_stage
    ADD CONSTRAINT artist_stage_ref_album_id_fkey FOREIGN KEY (ref_album_id) REFERENCES melo.album(id);


--
-- Name: artist_stage artist_stage_ref_artist_id_fkey; Type: FK CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.artist_stage
    ADD CONSTRAINT artist_stage_ref_artist_id_fkey FOREIGN KEY (ref_artist_id) REFERENCES melo.artist(id);


--
-- Name: artist_stage artist_stage_ref_track_id_fkey; Type: FK CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.artist_stage
    ADD CONSTRAINT artist_stage_ref_track_id_fkey FOREIGN KEY (ref_track_id) REFERENCES melo.track(id);


--
-- Name: related_artist related_artist_artist_id_fkey; Type: FK CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.related_artist
    ADD CONSTRAINT related_artist_artist_id_fkey FOREIGN KEY (artist_id) REFERENCES melo.artist(id);


--
-- Name: related_artist related_artist_related_artist_id_fkey; Type: FK CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.related_artist
    ADD CONSTRAINT related_artist_related_artist_id_fkey FOREIGN KEY (related_artist_id) REFERENCES melo.artist(id);


--
-- Name: track track_album_id_fkey; Type: FK CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.track
    ADD CONSTRAINT track_album_id_fkey FOREIGN KEY (album_id) REFERENCES melo.album(id);


--
-- Name: track_artist_name track_artist_name_artist_name_id_fkey; Type: FK CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.track_artist_name
    ADD CONSTRAINT track_artist_name_artist_name_id_fkey FOREIGN KEY (artist_name_id) REFERENCES melo.artist_name(id) ON DELETE CASCADE;


--
-- Name: track_artist_name track_artist_name_track_id_fkey; Type: FK CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.track_artist_name
    ADD CONSTRAINT track_artist_name_track_id_fkey FOREIGN KEY (track_id) REFERENCES melo.track(id) ON DELETE CASCADE;


--
-- Name: track_genre track_genre_genre_id_fkey; Type: FK CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.track_genre
    ADD CONSTRAINT track_genre_genre_id_fkey FOREIGN KEY (genre_id) REFERENCES melo.genre(id) ON DELETE CASCADE;


--
-- Name: track_genre track_genre_track_id_fkey; Type: FK CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.track_genre
    ADD CONSTRAINT track_genre_track_id_fkey FOREIGN KEY (track_id) REFERENCES melo.track(id) ON DELETE CASCADE;


--
-- Name: track track_source_id_fkey; Type: FK CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.track
    ADD CONSTRAINT track_source_id_fkey FOREIGN KEY (source_id) REFERENCES melo.source(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: track_stage track_stage_album_id_fkey; Type: FK CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.track_stage
    ADD CONSTRAINT track_stage_album_id_fkey FOREIGN KEY (album_id) REFERENCES melo.album(id);


--
-- Name: track_stage track_stage_ref_album_id_fkey; Type: FK CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.track_stage
    ADD CONSTRAINT track_stage_ref_album_id_fkey FOREIGN KEY (ref_album_id) REFERENCES melo.album(id);


--
-- Name: track_stage track_stage_ref_artist_id_fkey; Type: FK CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.track_stage
    ADD CONSTRAINT track_stage_ref_artist_id_fkey FOREIGN KEY (ref_artist_id) REFERENCES melo.artist(id);


--
-- Name: track_stage track_stage_ref_track_id_fkey; Type: FK CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.track_stage
    ADD CONSTRAINT track_stage_ref_track_id_fkey FOREIGN KEY (ref_track_id) REFERENCES melo.track(id);


--
-- Name: track_stage track_stage_source_id_fkey; Type: FK CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.track_stage
    ADD CONSTRAINT track_stage_source_id_fkey FOREIGN KEY (source_id) REFERENCES melo.source(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- PostgreSQL database dump complete
--


--
-- Dbmate schema migrations
--

INSERT INTO public.schema_migrations (version) VALUES
    ('20200505222518'),
    ('20200626210715');
