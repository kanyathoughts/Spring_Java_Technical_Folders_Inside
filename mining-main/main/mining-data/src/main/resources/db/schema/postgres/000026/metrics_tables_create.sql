CREATE TABLE "source_metrics" (
	"module" uuid NOT NULL REFERENCES "module" ON DELETE CASCADE,
	"physical_lines" int,
	"code_lines" int,
	"comment_lines" int,
	"complexity_mc_cabe" int,
	"dead_code_lines" int,
	PRIMARY KEY ("module")
);


CREATE TABLE "error_marker" (
	"project" uuid NOT NULL REFERENCES "project" ON DELETE CASCADE,
	"module" uuid REFERENCES "module" ON DELETE CASCADE,
	"severity" text,
	"key" text,
	"cause" text,
	"location" "module_location",
	"line" int
);

CREATE INDEX "error_marker_project" ON "error_marker" USING btree ("project");
CREATE INDEX "error_marker_module" ON "error_marker" USING btree ("module");


CREATE TABLE "module_dead_code" (
	"module" uuid NOT NULL REFERENCES "module" ON DELETE CASCADE,
	"starting_line" int,
	"number_of_lines" int,
	"dead_code" text
);

CREATE INDEX "module_dead_code_module" ON "module_dead_code" USING btree ("module");

