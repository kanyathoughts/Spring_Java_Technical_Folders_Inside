CREATE SEQUENCE "module_nid";

ALTER TABLE "module" ALTER COLUMN "nid" SET DEFAULT nextval('"module_nid"');
ALTER TABLE "module" ADD COLUMN "name" text;
ALTER TABLE "module" ADD COLUMN "path" text;
ALTER TABLE "module" ADD COLUMN "technology" text;
ALTER TABLE "module" ADD COLUMN "type" text;
ALTER TABLE "module" ADD COLUMN "storage" text;
ALTER TABLE "module" ADD COLUMN "origin" text;
ALTER TABLE "module" ADD COLUMN "creator" text;
ALTER TABLE "module" ADD COLUMN "identified" bool;
ALTER TABLE "module" ADD COLUMN "info" jsonb;
ALTER TABLE "module" ADD COLUMN "source" uuid REFERENCES "source" ON DELETE SET NULL;
ALTER TABLE "module" ADD COLUMN "content_hash" bytea;
ALTER TABLE "module" ADD COLUMN "link_hash" text;
ALTER TABLE "module" ADD COLUMN "location" "module_location";
ALTER TABLE "module" ADD COLUMN "representation" text;
ALTER TABLE "module" ADD COLUMN "requires_review" bool;
ALTER TABLE "module" ADD COLUMN "modified_date" "timestamp_zoned_milli" DEFAULT CURRENT_TIMESTAMP;

ALTER TABLE "module" ADD COLUMN "metrics_date" "timestamp_zoned_milli";
ALTER TABLE "module" ADD COLUMN "description" text;

CREATE INDEX "module_project_idx" ON "module" USING btree ("project");
CREATE INDEX "module_source_idx" ON "module" USING btree ("source");

CREATE UNIQUE INDEX "project_path_idx" on "module" ("project", LOWER("path"));
CREATE INDEX "module_link_hash_idx" ON module (project, link_hash);

CREATE INDEX "module_project_tech_idx" ON module (project, technology);
CREATE INDEX "module_project_name_tech_idx" ON module (project, name, technology);
CREATE INDEX "module_project_path_idx" ON module (project, path);