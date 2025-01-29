CREATE TABLE "source" (
	"id" uuid,
	"project" uuid REFERENCES "project" ON DELETE CASCADE,
	"content" bytea NOT NULL,
	PRIMARY KEY ("id")
);

CREATE SEQUENCE "source_nid";

CREATE TABLE "source_info" (
	"nid" bigint UNIQUE DEFAULT nextval('"source_nid"'),
	"path" text NOT NULL,
	"name" text NOT NULL,
	"technology" text NOT NULL,
	"type" text NOT NULL,
	"meta_data_revision" bigint NOT NULL DEFAULT 1,
	"content_revision" bigint NOT NULL DEFAULT 1,
	"content_hash" bytea NOT NULL,
	PRIMARY KEY ("uid"),
	FOREIGN KEY ("uid") REFERENCES "source" ON DELETE CASCADE
) INHERITS ("mining_entity");

CREATE INDEX "source_info_name_technology_idx" ON "source_info" USING btree  ("name", "technology");
CREATE INDEX "source_info_path_idx" ON source_info (path);

CREATE TABLE "source_references" (
	"src" uuid REFERENCES "source" ON DELETE CASCADE,
	"dst" uuid REFERENCES "source" ON DELETE CASCADE,
	"properties" jsonb,
	PRIMARY KEY ("src", "dst")
);

CREATE OR REPLACE FUNCTION module_location_substr(l module_location, t bytea) RETURNS bytea
	LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE
AS $BODY$
BEGIN
	RETURN substr(t, (l).offset + 1, (l).length);
END $BODY$;

CREATE OR REPLACE FUNCTION utf8_of_bytes(b bytea) RETURNS text
	LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE
AS $BODY$
BEGIN
	RETURN convert_from(b, 'UTF8');
EXCEPTION WHEN OTHERS THEN
	RETURN concat('[UNICODE ERROR ', sqlstate, ']');
END $BODY$;

CREATE OR REPLACE FUNCTION utf8_to_bytes(t text) RETURNS bytea
	LANGUAGE plpgsql IMMUTABLE STRICT PARALLEL SAFE
AS $BODY$
BEGIN
	RETURN convert_to(t, 'UTF8');
END $BODY$;
