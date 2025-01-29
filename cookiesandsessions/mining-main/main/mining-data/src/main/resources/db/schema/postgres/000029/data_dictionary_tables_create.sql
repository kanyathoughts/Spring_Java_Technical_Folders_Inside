CREATE TABLE "data_dictionary_other_scope" (
	"project" uuid REFERENCES "project" ON DELETE CASCADE,
	"name" text NOT NULL,
	PRIMARY KEY ("project", "name")
);

CREATE SEQUENCE "data_dictionary_nid";

CREATE TABLE "data_dictionary" (
	"nid" bigint UNIQUE DEFAULT nextval('"data_dictionary_nid"'),
	"module" uuid NOT NULL REFERENCES "module" ON DELETE CASCADE,
	"location" "module_location",
	"name" text NOT NULL,
	"description" text NOT NULL,
	"format" text,
	"scopes" jsonb,
	"length" bigint,
	"created_by" text NOT NULL,
	"updated_by" text,
	"pic_clause" text,
	"defined_location" text,
	"state" "working_state",
	"is_business" boolean NOT NULL DEFAULT false,
	"field_transformation" text,
	"source_input" text,
	"target_output" text,
	"is_referenced" boolean NOT NULL DEFAULT false,
	"usage" text,
	"is_candidate" boolean NOT NULL DEFAULT false,
	"field_level" bigint,
	"parent_group" text,
	"group_path" text,
	"indentation" bigint,
	"initial_value" text,
	PRIMARY KEY ("uid")
) INHERITS ("mining_entity");

CREATE TABLE "data_dictionary_annotations" (
	"entry" uuid REFERENCES "data_dictionary" ON DELETE CASCADE,
	"annotation" uuid REFERENCES "annotation" ON DELETE CASCADE,
	PRIMARY KEY ("entry", "annotation")
);
