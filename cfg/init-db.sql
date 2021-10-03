CREATE EXTENSION postgis;

CREATE TABLE IF NOT EXISTS trails (
    id          SERIAL PRIMARY KEY,
    name        TEXT NOT NULL,
    -- shortname   TEXT,
    -- tags     TEXT[] NOT NULL check (array_position(tags, NULL) is NULL),
    description TEXT,
    color       CHAR(7),
    sectioned   BOOLEAN NOT NULL DEFAULT FALSE
);

CREATE TABLE IF NOT EXISTS trail_sections (
    id          SERIAL PRIMARY KEY,
    trail_id    INT NOT NULL,
    -- maybe?: num         INT,
    geom        GEOGRAPHY(LINESTRING, 3006) NOT NULL,
    name        TEXT,
    description TEXT,
    FOREIGN KEY (trail_id)
        REFERENCES trails (id)
        ON DELETE CASCADE
);

CREATE TYPE LOCATIONTYPE AS ENUM (
    'water',
    'shelter',
    'wc',
    'cabin'
);

CREATE TABLE IF NOT EXISTS locations (
    id          SERIAL PRIMARY KEY,
    geom        GEOGRAPHY(POINT, 3006) NOT NULL,
    spot        LOCATIONTYPE           NOT NULL,
    name        TEXT                   NOT NULL,
    description TEXT                   NOT NULL
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
    geom        GEOGRAPHY(MULTIPOLYGON, 3006) NOT NULL,
    area        AREATYPE                      NOT NULL,
    name        TEXT                          NOT NULL,
    description TEXT                          NOT NULL
);

-- TODO: Figure out use case for TIGER
DROP SCHEMA tiger CASCADE;

CREATE USER tegola;
GRANT SELECT ON ALL TABLES IN SCHEMA public TO tegola;

-- Do this after insert:
CREATE INDEX IF NOT EXISTS trail_sections_idx ON trail_sections USING GIST (geom);
CLUSTER trail_sections_idx ON trail_sections;
