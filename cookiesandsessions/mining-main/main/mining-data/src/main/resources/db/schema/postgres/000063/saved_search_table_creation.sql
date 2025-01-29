CREATE SEQUENCE "saved_search_id";

CREATE TABLE "saved_search" (
	"id" bigint DEFAULT nextval('"saved_search_id"'),
	"client" uuid REFERENCES "client" ON DELETE CASCADE,
	"project" uuid REFERENCES "project" ON DELETE CASCADE,
	"name" text NOT NULL,
	"saved_search" text NOT NULL,
	"scope" text NOT NULL,
	"usage" text NOT NULL,
	"modifiers" text[],
	"created_by" text,
	PRIMARY KEY ("id"),
	UNIQUE ("name", "usage", "project", "client", "created_by")
);