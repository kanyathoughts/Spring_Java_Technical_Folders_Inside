CREATE TABLE "offline_token" (
	"id" uuid NOT NULL,
	"subject" text NOT NULL,
	"username" text NOT NULL,
	"description" text NOT NULL,
	"bearer_token" text NOT NULL,
	"refresh_token" text NOT NULL,
	"created" timestamp_zoned_milli NOT NULL DEFAULT CURRENT_TIMESTAMP,
	PRIMARY KEY ("id"),
	UNIQUE("bearer_token")
);
CREATE INDEX "offline_token_subject_idx" ON "offline_token" ("subject", "created" DESC);
