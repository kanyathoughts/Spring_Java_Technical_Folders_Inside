CREATE TABLE "custom_property" (
	id uuid DEFAULT gen_random_uuid(),
	project uuid NOT NULL REFERENCES "project" ON DELETE CASCADE,
	parent uuid REFERENCES "custom_property" ON DELETE CASCADE,
	ordinal int NOT NULL DEFAULT 0,
	name text NOT NULL,
	properties jsonb,
	PRIMARY KEY ("id")
);
CREATE INDEX custom_property_parent_idx ON "custom_property" USING btree (parent NULLS FIRST);
CREATE INDEX custom_property_project_idx ON "custom_property" USING btree (project);

CREATE TABLE "custom_property_entities" (
	project uuid REFERENCES "project" ON DELETE CASCADE,
	entity text,
	property uuid REFERENCES "custom_property" ON DELETE CASCADE,
	ordinal int NOT NULL DEFAULT 0,
	name text,
	properties jsonb,
	PRIMARY KEY ("project", "entity", "property")
);
CREATE INDEX custom_property_entities_property_idx ON "custom_property_entities" USING btree (property);

ALTER TABLE custom_enum ALTER COLUMN id SET DEFAULT gen_random_uuid();
