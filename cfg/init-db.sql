CREATE EXTENSION postgis;

CREATE TABLE IF NOT EXISTS trails (
    id          SERIAL PRIMARY KEY,
    name        TEXT NOT NULL,
    description TEXT,
    color       CHAR(6),
    sectioned   BOOLEAN NOT NULL DEFAULT FALSE
);

CREATE TABLE IF NOT EXISTS trail_sections (
    id          SERIAL PRIMARY KEY,
    trail_id    INT NOT NULL,
    geog        GEOGRAPHY(LINESTRING) NOT NULL,
    name        TEXT,
    description TEXT,
    FOREIGN KEY (trail_id)
        REFERENCES trails (id)
        ON DELETE CASCADE
);

CREATE TYPE SPOTTYPE AS ENUM (
    'water',
    'shelter',
    'wc',
    'cabin'
);

CREATE TABLE IF NOT EXISTS spots (
    id          SERIAL PRIMARY KEY,
    geog        GEOGRAPHY(POINT)    NOT NULL,
    spot        ICONTYPE            NOT NULL,
    name        TEXT                NOT NULL,
    description TEXT                NOT NULL,
    shown       BOOLEAN NOT NULL DEFAULT TRUE
);

CREATE TYPE AREATYPE AS ENUM (
    'national park',
    'nature reserve',
    'municipality',
    'county',
    'province'
);

CREATE TABLE IF NOT EXISTS areas (
    id          SERIAL PRIMARY KEY,
    geog        GEOGRAPHY(MULTIPOLYGON) NOT NULL,
    area        AREATYPE                NOT NULL,
    name        TEXT                    NOT NULL,
    description TEXT                    NOT NULL
);

-- TODO: Figure out use case for TIGER
DROP SCHEMA tiger CASCADE;

-- Do this after insert:
--
--      CREATE INDEX IF NOT EXISTS trail_sections_idx ON trail_sections USING GIST (geom);
--      CLUSTER trail_sections_idx ON trail_sections;
--
