CREATE SEQUENCE "statement_nid";

CREATE TABLE "statement" (
	"nid" bigint UNIQUE DEFAULT nextval('"statement_nid"'),
	"module" uuid NOT NULL REFERENCES "module" ON DELETE CASCADE,
	"technology" text NOT NULL,
	"type" text NOT NULL,
	"text" text NOT NULL,
	"properties" jsonb,
	PRIMARY KEY (uid)
) INHERITS ("mining_entity");

CREATE INDEX "statement_module" ON "statement" USING btree ("module");
