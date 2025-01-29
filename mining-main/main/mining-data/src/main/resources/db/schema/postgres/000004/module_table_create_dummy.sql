CREATE TABLE "module" (
	"nid" bigint UNIQUE,
	"project" uuid NOT NULL REFERENCES "project" ON DELETE CASCADE,
	PRIMARY KEY ("uid")
) INHERITS ("mining_entity");
