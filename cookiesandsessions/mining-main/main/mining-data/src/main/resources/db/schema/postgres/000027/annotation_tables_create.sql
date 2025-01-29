CREATE SEQUENCE "annotation_category_id";

CREATE TABLE "annotation_category" (
	"id" bigint DEFAULT nextval('"annotation_category_id"'),
	"project" uuid NOT NULL REFERENCES "project" ON DELETE CASCADE,
	"name" text NOT NULL,
	"types" text[] NOT NULL,
	PRIMARY KEY ("id")
);

CREATE UNIQUE INDEX "annotation_category_name_unique" ON "annotation_category" ("project", "name", "types");

CREATE SEQUENCE "annotation_nid";

CREATE TABLE "annotation" (
	"nid" bigint UNIQUE DEFAULT nextval('"annotation_nid"'),
	"module" uuid NOT NULL REFERENCES "module" ON DELETE CASCADE,
	"location" "module_location",
	"name" text NOT NULL,
	"state" "working_state" NOT NULL,
	"type" text NOT NULL,
	"category" bigint REFERENCES "annotation_category" ON DELETE SET NULL,
	"created_by" text NOT NULL,
	"updated_by" text,
	"source" bytea,
	"reasons" text[],
	"translation" text,
	PRIMARY KEY ("uid")
) INHERITS ("mining_entity");
