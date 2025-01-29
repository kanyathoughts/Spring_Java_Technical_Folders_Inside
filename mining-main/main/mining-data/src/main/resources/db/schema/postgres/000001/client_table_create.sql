CREATE SEQUENCE "client_nid";

CREATE TABLE "client" (
	"nid" bigint UNIQUE DEFAULT nextval('"client_nid"'),
	"name" text UNIQUE,
	"logo" "binary_attachment",
	"to_be_deleted" bool NOT NULL DEFAULT false,
	PRIMARY KEY ("uid")
) INHERITS ("mining_entity");
