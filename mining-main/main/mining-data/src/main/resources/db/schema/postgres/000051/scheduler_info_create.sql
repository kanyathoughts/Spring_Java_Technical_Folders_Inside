CREATE TYPE "scheduler_type" AS ENUM
    ('Control-M', 'CA7');

CREATE TYPE "scheduler_entry_type" AS ENUM
    ('folder', 'job', 'table', 'condition', 'unknown');

CREATE TABLE "scheduler_import"
(
    "uid" uuid NOT NULL,
    "project" uuid NOT NULL,
    "scheduler_type" "scheduler_type" NOT NULL,
    "scheduler_version" text NOT NULL DEFAULT '',
    "identifier" text NOT NULL DEFAULT '',
    "source" uuid NOT NULL,
    "importer_used" text NOT NULL DEFAULT 'default'::text,
    "imported_on" timestamp_zoned_milli NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "description" text NOT NULL DEFAULT '',
    CONSTRAINT "scheduler_import_pkey" PRIMARY KEY (uid),
    CONSTRAINT "scheduler_import_project_fkey" FOREIGN KEY ("project")
        REFERENCES "project" ("uid") MATCH SIMPLE
        ON UPDATE CASCADE
        ON DELETE CASCADE,
    CONSTRAINT "scheduler_import_source_fkey" FOREIGN KEY ("source")
        REFERENCES "source" ("id") MATCH SIMPLE
        ON UPDATE CASCADE
        ON DELETE CASCADE
);

CREATE INDEX "scheduler_import_project_idx" ON "scheduler_import" ("project");
CREATE UNIQUE INDEX "scheduler_import_project_identifier_idx" ON "scheduler_import" ("project", "identifier");

CREATE TABLE "scheduler_entry"
(
    "uid" uuid NOT NULL,
    "type" "scheduler_entry_type" NOT NULL,
    "scheduler_import" uuid NOT NULL,
    "content" jsonb NOT NULL,
    "identifier" text NOT NULL DEFAULT '',
    "module" uuid,
    "contained_in" uuid,
    CONSTRAINT "scheduler_entry_pkey" PRIMARY KEY ("uid"),
    CONSTRAINT "scheduler_entry_contained_in" FOREIGN KEY ("contained_in")
        REFERENCES "scheduler_entry" ("uid") MATCH SIMPLE
        ON UPDATE CASCADE
        ON DELETE CASCADE
        NOT VALID,
    CONSTRAINT "scheduler_entry_import_fkey" FOREIGN KEY ("scheduler_import")
        REFERENCES "scheduler_import" ("uid") MATCH SIMPLE
        ON UPDATE CASCADE
        ON DELETE CASCADE,
    CONSTRAINT "scheduler_entry_module_fkey" FOREIGN KEY ("module")
        REFERENCES "module" ("uid") MATCH SIMPLE
        ON UPDATE CASCADE
        ON DELETE SET NULL
);

CREATE INDEX "scheduler_entry_import_type_idx" ON "scheduler_entry" ("scheduler_import");

CREATE INDEX "scheduler_entry_module_idx" ON "scheduler_entry" ("module");

CREATE INDEX "scheduler_entry_name" ON "scheduler_entry" (("content" ->> 'NAME'));

CREATE TABLE "scheduler_entry_relationship"
(
    "uid" uuid NOT NULL,
    "predecessor" uuid NOT NULL,
    "successor" uuid NOT NULL,
    "is_ok" boolean,
    "identifier" text,
    CONSTRAINT "scheduler_entry_relationship_pkey" PRIMARY KEY ("uid"),
    CONSTRAINT "scheduler_entry_relationship_predecessor_fkey" FOREIGN KEY ("predecessor")
        REFERENCES "scheduler_entry" ("uid") MATCH SIMPLE
        ON UPDATE CASCADE
        ON DELETE CASCADE,
    CONSTRAINT "scheduler_entry_relationship_successor_fkey" FOREIGN KEY ("successor")
        REFERENCES "scheduler_entry" ("uid") MATCH SIMPLE
        ON UPDATE CASCADE
        ON DELETE CASCADE
);

CREATE INDEX "scheduler_entry_relationship_predecessor_idx" ON "scheduler_entry_relationship" ("predecessor");

CREATE INDEX "scheduler_entry_relationship_successor_idx" ON "scheduler_entry_relationship" ("successor");
