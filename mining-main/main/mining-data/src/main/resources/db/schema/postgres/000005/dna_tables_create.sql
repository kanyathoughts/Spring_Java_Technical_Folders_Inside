CREATE TABLE public."dna_string" (
	"module" uuid REFERENCES "module" ON DELETE CASCADE,
	"sequencer" text NOT NULL,
	"generated" timestamp_zoned_milli NOT NULL,
	"content_hash" bytea NOT NULL,
	PRIMARY KEY ("module", "sequencer")
);

CREATE TABLE public."dna_string_element" (
	"module" uuid NOT NULL,
	"sequencer" text NOT NULL,
	"index" int NOT NULL,
	"location" "module_location" NOT NULL,
	"value" text NOT NULL,
	PRIMARY KEY ("module", "sequencer", "index"),
	FOREIGN KEY ("module", "sequencer") REFERENCES "dna_string" ON DELETE CASCADE
);

CREATE TABLE public."dna_similarity" (
	"a_module" uuid NOT NULL,
	"b_module" uuid NOT NULL,
	"sequencer" text NOT NULL,
	"similarity_algo" text NOT NULL,
	"similarity" double precision NOT NULL,
	PRIMARY KEY ("a_module", "b_module", "sequencer", "similarity_algo"),
	FOREIGN KEY ("a_module", "sequencer") REFERENCES "dna_string" ON DELETE CASCADE,
	FOREIGN KEY ("b_module", "sequencer") REFERENCES "dna_string" ON DELETE CASCADE
);
CREATE INDEX dna_similarity_a_module ON dna_similarity (a_module, sequencer);
CREATE INDEX dna_similarity_b_module ON dna_similarity (b_module, sequencer);

CREATE TABLE public."dna_snapshot" (
	"id" uuid UNIQUE,
	"project" uuid REFERENCES "project" ON DELETE CASCADE,
	"name" text NOT NULL DEFAULT '',
	"updated" timestamp_zoned_milli NOT NULL DEFAULT current_timestamp,
	"module_count" integer NOT NULL,
	"dna_config" jsonb NOT NULL,
	PRIMARY KEY ("id")
);

CREATE TABLE public."dna_community" (
	"id" uuid UNIQUE,
	"snapshot" uuid REFERENCES "dna_snapshot" ON DELETE CASCADE,
	"sequencer" text NOT NULL,
	"similarity_algo" text NOT NULL,
	"cluster_algo" text NOT NULL,
	"cluster_index" int NOT NULL,
	"title" text,
	PRIMARY KEY ("id")
);

CREATE TABLE public."dna_community_modules" (
	"community" uuid REFERENCES "dna_community" ON DELETE CASCADE,
	"module" uuid REFERENCES "module" ON DELETE CASCADE,
	PRIMARY KEY ("community", "module")
);
