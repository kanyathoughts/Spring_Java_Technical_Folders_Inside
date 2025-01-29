CREATE TABLE "custom_enum" (
	"id" uuid,
	"project" uuid REFERENCES "project" ON DELETE CASCADE,
	"name" text,
	"properties" jsonb,
	PRIMARY KEY ("id")
);

CREATE UNIQUE INDEX "custom_enum_project_name_unique" ON "custom_enum" ("project", "name");

CREATE TABLE "custom_enum_values" (
	"enum" uuid REFERENCES "custom_enum" ON DELETE CASCADE,
	"value" text,
	"ordinal" int NOT NULL DEFAULT 0,
	"properties" jsonb,
	PRIMARY KEY ("enum", "value")
);
