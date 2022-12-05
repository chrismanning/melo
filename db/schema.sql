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
-- Name: numeric; Type: COLLATION; Schema: melo; Owner: -
--

CREATE COLLATION melo."numeric" (provider = icu, locale = 'en@colNumeric=yes');


--
-- Name: intervalrange; Type: TYPE; Schema: melo; Owner: -
--

CREATE TYPE melo.intervalrange AS RANGE (
    subtype = interval,
    multirange_type_name = melo.intervalmultirange
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
-- Name: attachment; Type: TABLE; Schema: melo; Owner: -
--

CREATE TABLE melo.attachment (
    id uuid DEFAULT melo.uuid_generate_v4() NOT NULL,
    uri text NOT NULL,
    kind text NOT NULL
);


--
-- Name: collection; Type: TABLE; Schema: melo; Owner: -
--

CREATE TABLE melo.collection (
    id uuid DEFAULT melo.uuid_generate_v4() NOT NULL,
    root_uri text NOT NULL,
    name text NOT NULL,
    watch boolean NOT NULL,
    kind text NOT NULL
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
-- Name: schema_migrations; Type: TABLE; Schema: melo; Owner: -
--

CREATE TABLE melo.schema_migrations (
    version character varying(255) NOT NULL
);


--
-- Name: source; Type: TABLE; Schema: melo; Owner: -
--

CREATE TABLE melo.source (
    id uuid DEFAULT melo.uuid_generate_v4() NOT NULL,
    kind text NOT NULL,
    metadata_format text NOT NULL,
    metadata jsonb NOT NULL,
    source_uri text NOT NULL COLLATE melo."numeric",
    idx bigint NOT NULL,
    time_range melo.intervalrange,
    scanned timestamp without time zone,
    collection_id uuid NOT NULL,
    cover text
);


--
-- Name: source_attachment; Type: TABLE; Schema: melo; Owner: -
--

CREATE TABLE melo.source_attachment (
    source_id uuid NOT NULL,
    attachment_id uuid NOT NULL
);


--
-- Name: tag_mapping; Type: TABLE; Schema: melo; Owner: -
--

CREATE TABLE melo.tag_mapping (
    name text NOT NULL,
    field_mappings jsonb NOT NULL
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
    source_id uuid NOT NULL,
    musicbrainz_id text
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
-- Name: attachment attachment_pk; Type: CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.attachment
    ADD CONSTRAINT attachment_pk PRIMARY KEY (id);


--
-- Name: collection collection_pk; Type: CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.collection
    ADD CONSTRAINT collection_pk PRIMARY KEY (id);


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
-- Name: schema_migrations schema_migrations_pkey; Type: CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.schema_migrations
    ADD CONSTRAINT schema_migrations_pkey PRIMARY KEY (version);


--
-- Name: source_attachment source_attachment_pk; Type: CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.source_attachment
    ADD CONSTRAINT source_attachment_pk PRIMARY KEY (source_id, attachment_id);


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
-- Name: tag_mapping tag_mapping_pk; Type: CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.tag_mapping
    ADD CONSTRAINT tag_mapping_pk PRIMARY KEY (name);


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
-- Name: album_musicbrainz_id_uindex; Type: INDEX; Schema: melo; Owner: -
--

CREATE UNIQUE INDEX album_musicbrainz_id_uindex ON melo.album USING btree (musicbrainz_id);


--
-- Name: artist_musicbrainz_id_uindex; Type: INDEX; Schema: melo; Owner: -
--

CREATE UNIQUE INDEX artist_musicbrainz_id_uindex ON melo.artist USING btree (musicbrainz_id);


--
-- Name: artist_name_artist_id_name_uindex; Type: INDEX; Schema: melo; Owner: -
--

CREATE UNIQUE INDEX artist_name_artist_id_name_uindex ON melo.artist_name USING btree (artist_id, name);


--
-- Name: artist_name_musicbrainz_id_uindex; Type: INDEX; Schema: melo; Owner: -
--

CREATE UNIQUE INDEX artist_name_musicbrainz_id_uindex ON melo.artist USING btree (name) WHERE (musicbrainz_id IS NOT NULL);


--
-- Name: artist_name_uindex; Type: INDEX; Schema: melo; Owner: -
--

CREATE UNIQUE INDEX artist_name_uindex ON melo.artist USING btree (name) WHERE (musicbrainz_id IS NULL);


--
-- Name: attachment_id_uindex; Type: INDEX; Schema: melo; Owner: -
--

CREATE UNIQUE INDEX attachment_id_uindex ON melo.attachment USING btree (id);


--
-- Name: attachment_uri_uindex; Type: INDEX; Schema: melo; Owner: -
--

CREATE UNIQUE INDEX attachment_uri_uindex ON melo.attachment USING btree (uri);


--
-- Name: collection_id_uindex; Type: INDEX; Schema: melo; Owner: -
--

CREATE UNIQUE INDEX collection_id_uindex ON melo.collection USING btree (id);


--
-- Name: collection_name_uindex; Type: INDEX; Schema: melo; Owner: -
--

CREATE UNIQUE INDEX collection_name_uindex ON melo.collection USING btree (name);


--
-- Name: collection_root_uri_uindex; Type: INDEX; Schema: melo; Owner: -
--

CREATE UNIQUE INDEX collection_root_uri_uindex ON melo.collection USING btree (root_uri);


--
-- Name: source_attachment_attachment_id_index; Type: INDEX; Schema: melo; Owner: -
--

CREATE INDEX source_attachment_attachment_id_index ON melo.source_attachment USING btree (attachment_id);


--
-- Name: source_attachment_source_id_index; Type: INDEX; Schema: melo; Owner: -
--

CREATE INDEX source_attachment_source_id_index ON melo.source_attachment USING btree (source_id);


--
-- Name: source_collection_id_index; Type: INDEX; Schema: melo; Owner: -
--

CREATE INDEX source_collection_id_index ON melo.source USING btree (collection_id);


--
-- Name: tag_mapping_name_uindex; Type: INDEX; Schema: melo; Owner: -
--

CREATE UNIQUE INDEX tag_mapping_name_uindex ON melo.tag_mapping USING btree (name);


--
-- Name: track_musicbrainz_id_uindex; Type: INDEX; Schema: melo; Owner: -
--

CREATE UNIQUE INDEX track_musicbrainz_id_uindex ON melo.track USING btree (musicbrainz_id);


--
-- Name: track_source_id_uindex; Type: INDEX; Schema: melo; Owner: -
--

CREATE UNIQUE INDEX track_source_id_uindex ON melo.track USING btree (source_id);


--
-- Name: track_track_number_disc_number_album_id_uindex; Type: INDEX; Schema: melo; Owner: -
--

CREATE UNIQUE INDEX track_track_number_disc_number_album_id_uindex ON melo.track USING btree (track_number, disc_number, album_id);


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
    ADD CONSTRAINT artist_name_artist_id_fkey FOREIGN KEY (artist_id) REFERENCES melo.artist(id) ON DELETE CASCADE;


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
-- Name: source_attachment source_attachment_attachment_id_fk; Type: FK CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.source_attachment
    ADD CONSTRAINT source_attachment_attachment_id_fk FOREIGN KEY (attachment_id) REFERENCES melo.attachment(id) ON DELETE CASCADE;


--
-- Name: source_attachment source_attachment_source_id_fk; Type: FK CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.source_attachment
    ADD CONSTRAINT source_attachment_source_id_fk FOREIGN KEY (source_id) REFERENCES melo.source(id) ON DELETE CASCADE;


--
-- Name: source source_collection_id_fk; Type: FK CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.source
    ADD CONSTRAINT source_collection_id_fk FOREIGN KEY (collection_id) REFERENCES melo.collection(id) ON DELETE CASCADE;


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
-- PostgreSQL database dump complete
--


--
-- Dbmate schema migrations
--

INSERT INTO melo.schema_migrations (version) VALUES
    ('20200505222518'),
    ('20200626210715'),
    ('20201129174644'),
    ('20201204000250'),
    ('20210620173517'),
    ('20211209202153'),
    ('20220712201124'),
    ('20220802210615'),
    ('20220920222818'),
    ('20221004171024'),
    ('20221027210121'),
    ('20221028182923'),
    ('20221030214943'),
    ('20221116005520'),
    ('20221204172930');
