
CREATE SEQUENCE "taxonomy_category_id";

CREATE TABLE "taxonomy_category" (
	"id" bigint DEFAULT nextval('"taxonomy_category_id"'),
	"project" uuid NOT NULL REFERENCES "project" ON DELETE CASCADE,
	"name" text NOT NULL,
	PRIMARY KEY ("id"),
	UNIQUE ("project", "name")
);

CREATE TABLE "taxonomy_type" (
	"id" uuid,
	"project" uuid NOT NULL REFERENCES "project" ON DELETE CASCADE,
	"category" bigint NOT NULL REFERENCES "taxonomy_category" ON DELETE CASCADE,
	"name" text NOT NULL,
	PRIMARY KEY ("id"),
	UNIQUE ("project", "name")
);

CREATE SEQUENCE "taxonomy_nid";

CREATE TABLE "taxonomy" (
	"nid" bigint UNIQUE DEFAULT nextval('"taxonomy_nid"'),
	"project" uuid NOT NULL REFERENCES "project" ON DELETE CASCADE,
	"type" uuid NOT NULL REFERENCES "taxonomy_type" ON DELETE CASCADE,
	"name" text NOT NULL,
	PRIMARY KEY ("uid"),
	UNIQUE ("project", "type", "name")
) INHERITS ("mining_entity");

CREATE TABLE "module_taxonomies" (
	"module" uuid REFERENCES "module" ON DELETE CASCADE,
	"taxonomy" uuid REFERENCES "taxonomy" ON DELETE CASCADE,
	"properties" jsonb,
	PRIMARY KEY ("module", "taxonomy")
);
