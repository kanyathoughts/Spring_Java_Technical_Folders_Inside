CREATE TABLE "module_undiscovered" (
	"project" uuid NOT NULL REFERENCES "project" ON DELETE CASCADE,
	"name" text,
	"path" text,
	PRIMARY KEY ("project", "name", "path")
);