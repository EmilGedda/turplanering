CREATE EXTENSION postgis;

CREATE TABLE IF NOT EXISTS trails (
    id          SERIAL PRIMARY KEY,
    name        TEXT NOT NULL,
    description TEXT,
    color       TEXT
);

CREATE TABLE IF NOT EXISTS trail_sections (
    id          SERIAL PRIMARY KEY,
    trail_id    INT NOT NULL,
    name        TEXT,
    description TEXT,
    geog        GEOGRAPHY(LINESTRING) NOT NULL,
    FOREIGN KEY (trail_id)
        REFERENCES trails (id)
        ON DELETE CASCADE
);

CREATE TYPE ICONTYPE AS ENUM (
    'water',
    'shelter',
    'wc',
    'cabin'
);

CREATE TABLE IF NOT EXISTS icons (
    icon        ICONTYPE NOT NULL,
    name        TEXT,
    description TEXT,
    geog        GEOGRAPHY(POINT) NOT NULL,
    shown       BOOLEAN DEFAULT TRUE
);

CREATE TYPE AREATYPE AS ENUM (
    'national park',
    'nature reserve',
    'municipality',
    'county',
    'province'
);

CREATE TABLE IF NOT EXISTS areas (
    area        AREATYPE NOT NULL,
    name        TEXT,
    description TEXT,
    geog        GEOGRAPHY(POLYGON) NOT NULL
);

-- TODO: Figure out use case for TIGER
DROP SCHEMA tiger CASCADE;

-- Do this after insert:
--
--      CREATE INDEX IF NOT EXISTS trail_sections_idx ON trail_sections USING GIST (geom);
--      CLUSTER trail_sections_idx ON trail_sections;
--
