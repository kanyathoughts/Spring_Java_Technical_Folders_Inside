CREATE TABLE "functional_block" (
	"project" uuid REFERENCES "project" ON DELETE CASCADE,
	"name" text NOT NULL DEFAULT '',
	"description" text NOT NULL DEFAULT '',
	"updated" timestamp_zoned_milli NOT NULL DEFAULT current_timestamp,
	PRIMARY KEY ("uid")
) INHERITS ("mining_entity");
CREATE INDEX "functional_block_project" ON "functional_block" ("project");

CREATE TABLE "functional_block_children" (
	"parent" uuid REFERENCES "functional_block" ON DELETE CASCADE,
	"child" uuid REFERENCES "functional_block" ON DELETE CASCADE,
	PRIMARY KEY ("parent", "child")
);
CREATE INDEX "functional_block_parent" ON "functional_block_children" ("parent");
CREATE INDEX "functional_block_child" ON "functional_block_children" ("child");

CREATE TABLE "functional_block_module_part" (
	"functional_block" uuid REFERENCES "functional_block" ON DELETE CASCADE,
    "module_link_hash" text NOT NULL,
    "location" "module_location",
    UNIQUE ("functional_block", "module_link_hash", "location")
);
CREATE INDEX "functional_block_module_part_functional_block" ON "functional_block_module_part" ("functional_block");

CREATE TABLE "functional_block_resolved_module_part" (
	"functional_block" uuid REFERENCES "functional_block" ON DELETE CASCADE,
	"module" uuid REFERENCES "module" ON DELETE CASCADE,
    "location" "module_location",
    UNIQUE ("functional_block", "module", "location")
);
CREATE INDEX "functional_block_resolved_module_part_functional_block" ON "functional_block_resolved_module_part" ("functional_block");
CREATE INDEX "functional_block_resolved_module_part_module" ON "functional_block_resolved_module_part" ("module");
