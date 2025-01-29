CREATE SEQUENCE "project_nid";

CREATE TABLE "project" (
	"client" uuid REFERENCES "client" ON DELETE CASCADE,
	"nid" bigint UNIQUE DEFAULT nextval('"project_nid"'), 
	"name" text,
	"to_be_deleted" bool NOT NULL DEFAULT false,
	"source_code_revision" bigint,
	"metrics_base_revision" bigint,
	"metrics_version" text,
	"metrics_date" timestamp_zoned_milli,
	"search_orders" jsonb[],
	"default_taxonomy_category" bigint,
	"technical_taxonomy_category" bigint,
	"custom_property_classes" jsonb,
	PRIMARY KEY ("uid")
) INHERITS ("mining_entity");

CREATE UNIQUE INDEX "project_client_name_unique" ON "project" ("client", "name");

CREATE TABLE "project_configuration" (
	"project" uuid REFERENCES "project" ON DELETE CASCADE,
	"name" text,
	"value" text,
	PRIMARY KEY ("project", "name")
);
