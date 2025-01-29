CREATE TABLE "field_info" (
	"id" uuid NOT NULL,
	"module" uuid NOT NULL REFERENCES "module" ON DELETE CASCADE,
	"ordinal" int NOT NULL,
	"name" text NOT NULL,
	"reference" text,
	"comment" text,
	"properties" jsonb,
	PRIMARY KEY ("id")
);

CREATE INDEX "field_info_module_ordinal_idx" ON "field_info" ("module", "ordinal");
