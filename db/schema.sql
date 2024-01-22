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
    kind text NOT NULL,
    rescan boolean DEFAULT true,
    library boolean DEFAULT false NOT NULL
);


--
-- Name: config; Type: TABLE; Schema: melo; Owner: -
--

CREATE TABLE melo.config (
    key text NOT NULL,
    value jsonb NOT NULL
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
-- Name: genre_parent; Type: TABLE; Schema: melo; Owner: -
--

CREATE TABLE melo.genre_parent (
    genre_id uuid NOT NULL,
    parent_genre uuid NOT NULL
);


--
-- Name: related_artist; Type: TABLE; Schema: melo; Owner: -
--

CREATE TABLE melo.related_artist (
    artist_id uuid NOT NULL,
    related_artist_id uuid NOT NULL
);


--
-- Name: release; Type: TABLE; Schema: melo; Owner: -
--

CREATE TABLE melo.release (
    id uuid DEFAULT melo.uuid_generate_v4() NOT NULL,
    title text NOT NULL,
    comment text,
    year_released text,
    length interval,
    musicbrainz_id text,
    original_year_released text,
    musicbrainz_group_id text,
    catalogue_number text,
    kind text DEFAULT 'album'::text NOT NULL
);


--
-- Name: release_artist_name; Type: TABLE; Schema: melo; Owner: -
--

CREATE TABLE melo.release_artist_name (
    release_id uuid NOT NULL,
    artist_name_id uuid NOT NULL
);


--
-- Name: release_genre; Type: TABLE; Schema: melo; Owner: -
--

CREATE TABLE melo.release_genre (
    release_id uuid NOT NULL,
    genre_id uuid NOT NULL
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
    release_id uuid NOT NULL,
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
-- Name: config config_pk; Type: CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.config
    ADD CONSTRAINT config_pk PRIMARY KEY (key);


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
-- Name: release_artist_name release_artist_name_pkey; Type: CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.release_artist_name
    ADD CONSTRAINT release_artist_name_pkey PRIMARY KEY (release_id, artist_name_id);


--
-- Name: release_genre release_genre_pkey; Type: CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.release_genre
    ADD CONSTRAINT release_genre_pkey PRIMARY KEY (release_id, genre_id);


--
-- Name: release release_pkey; Type: CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.release
    ADD CONSTRAINT release_pkey PRIMARY KEY (id);


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

CREATE UNIQUE INDEX artist_name_musicbrainz_id_uindex ON melo.artist USING btree (name, musicbrainz_id) WHERE (musicbrainz_id IS NOT NULL);


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
-- Name: parent_genre_genre_id_index; Type: INDEX; Schema: melo; Owner: -
--

CREATE INDEX parent_genre_genre_id_index ON melo.genre_parent USING btree (genre_id);


--
-- Name: parent_genre_parent_genre_index; Type: INDEX; Schema: melo; Owner: -
--

CREATE INDEX parent_genre_parent_genre_index ON melo.genre_parent USING btree (parent_genre);


--
-- Name: release_musicbrainz_group_id_uindex; Type: INDEX; Schema: melo; Owner: -
--

CREATE UNIQUE INDEX release_musicbrainz_group_id_uindex ON melo.release USING btree (musicbrainz_group_id) WHERE (musicbrainz_id IS NULL);


--
-- Name: release_musicbrainz_id_uindex; Type: INDEX; Schema: melo; Owner: -
--

CREATE UNIQUE INDEX release_musicbrainz_id_uindex ON melo.release USING btree (musicbrainz_id);


--
-- Name: release_title_year_released_uindex; Type: INDEX; Schema: melo; Owner: -
--

CREATE UNIQUE INDEX release_title_year_released_uindex ON melo.release USING btree (title, year_released) WHERE ((musicbrainz_id IS NULL) AND (musicbrainz_group_id IS NULL));


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
-- Name: track_track_number_disc_number_release_id_uindex; Type: INDEX; Schema: melo; Owner: -
--

CREATE UNIQUE INDEX track_track_number_disc_number_release_id_uindex ON melo.track USING btree (track_number, disc_number, release_id);


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
-- Name: genre_parent parent_genre_genre_fk; Type: FK CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.genre_parent
    ADD CONSTRAINT parent_genre_genre_fk FOREIGN KEY (parent_genre) REFERENCES melo.genre(id) ON DELETE CASCADE;


--
-- Name: genre_parent parent_genre_id_genre_fk; Type: FK CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.genre_parent
    ADD CONSTRAINT parent_genre_id_genre_fk FOREIGN KEY (genre_id) REFERENCES melo.genre(id) ON DELETE CASCADE;


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
-- Name: release_artist_name release_artist_name_artist_name_id_fkey; Type: FK CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.release_artist_name
    ADD CONSTRAINT release_artist_name_artist_name_id_fkey FOREIGN KEY (artist_name_id) REFERENCES melo.artist_name(id) ON DELETE CASCADE;


--
-- Name: release_artist_name release_artist_name_release_id_fkey; Type: FK CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.release_artist_name
    ADD CONSTRAINT release_artist_name_release_id_fkey FOREIGN KEY (release_id) REFERENCES melo.release(id) ON DELETE CASCADE;


--
-- Name: release_genre release_genre_genre_id_fkey; Type: FK CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.release_genre
    ADD CONSTRAINT release_genre_genre_id_fkey FOREIGN KEY (genre_id) REFERENCES melo.genre(id) ON DELETE CASCADE;


--
-- Name: release_genre release_genre_release_id_fkey; Type: FK CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.release_genre
    ADD CONSTRAINT release_genre_release_id_fkey FOREIGN KEY (release_id) REFERENCES melo.release(id) ON DELETE CASCADE;


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
-- Name: track track_release_id_fkey; Type: FK CONSTRAINT; Schema: melo; Owner: -
--

ALTER TABLE ONLY melo.track
    ADD CONSTRAINT track_release_id_fkey FOREIGN KEY (release_id) REFERENCES melo.release(id);


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
    ('20221204172930'),
    ('20221208162831'),
    ('20221209154929'),
    ('20221211232034'),
    ('20230219160659'),
    ('20230422225419'),
    ('20230708170753'),
    ('20231216191423'),
    ('20231216215146');
