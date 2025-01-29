CREATE TYPE "effort_summary_type" AS ENUM (
	'TYPE',
	'PRICING'
);

CREATE TABLE "effort_summary" (
    "project" uuid NOT NULL REFERENCES "project" ON DELETE CASCADE,
    "index" bigint NOT NULL,
    "type" "effort_summary_type" NOT NULL,
    "properties" jsonb NOT NULL,
    PRIMARY KEY (project, type, index)
);