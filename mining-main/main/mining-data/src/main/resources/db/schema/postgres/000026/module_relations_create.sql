CREATE TYPE "module_relationship_type" AS ENUM (
	'NONE',
	'INCLUDES',
	'REFERENCES',
	'CALLS',
	'ACCESSES',
	'CONTAINS'
);

CREATE TABLE "module_relationship" (
	"id" UUID,
	"src" uuid NOT NULL REFERENCES "module" ON DELETE CASCADE,
	"src_location" "module_location",
	"dst" uuid NOT NULL REFERENCES "module" ON DELETE CASCADE,
	"dst_location" "module_location",
	"type" "module_relationship_type" NOT NULL,
	"properties" jsonb,
	"dependency_binding" text,
	"dependency_attributes" text,
	PRIMARY KEY ("id")
);
CREATE INDEX "module_relationship_src_type" ON "module_relationship" USING btree ("src", "type");
CREATE INDEX "module_relationship_dst_type" ON "module_relationship" USING btree ("dst", "type");
CREATE INDEX "module_relationship_src_dst_type" ON "module_relationship" USING btree ("src", "dst", "type");


CREATE TABLE "module_conditional_relationship" (
	"module_relationship" uuid NOT NULL,
	"reached_from_module" uuid NOT NULL,
	PRIMARY KEY ("module_relationship", "reached_from_module"),
	FOREIGN KEY ("module_relationship") REFERENCES "module_relationship" ON DELETE CASCADE,
	FOREIGN KEY ("reached_from_module") REFERENCES "module" ON DELETE CASCADE
);

CREATE INDEX "module_conditional_relationship_reference" ON "module_conditional_relationship" USING btree ("module_relationship");


CREATE TABLE "dependency_definition" (
	"id" uuid NOT NULL,
	"module" uuid NOT NULL REFERENCES "module" ON DELETE CASCADE,
	"attributes" jsonb NOT NULL,
	"binding_type" text NOT NULL,
	"location" "module_location",
	"module_filters" jsonb[] NOT NULL,
	"type" "module_relationship_type" NOT NULL,
	"resolution_flags" text[] NOT NULL,
	"resolved" bool NOT NULL,
	"reached_from_modules" jsonb[],
	PRIMARY KEY ("id")
);

CREATE INDEX "dependency_definition_module" ON "dependency_definition" USING btree ("module");
