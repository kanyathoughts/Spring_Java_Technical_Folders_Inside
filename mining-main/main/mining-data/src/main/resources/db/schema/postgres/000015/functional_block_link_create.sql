CREATE TABLE "functional_block_link_condition" (
    "uid" uuid,
    "label" text,
    PRIMARY KEY ("uid")
);

CREATE TABLE "functional_block_link" (
    "uid" uuid,
    "parent" uuid NOT NULL REFERENCES "functional_block" ON DELETE CASCADE,
    "child_a" uuid NOT NULL REFERENCES "functional_block" ON DELETE CASCADE,
    "child_b" uuid NOT NULL REFERENCES "functional_block" ON DELETE CASCADE,
    "condition" uuid REFERENCES "functional_block_link_condition" ON DELETE SET NULL,
    "condition_label" text,
    "flags" jsonb,
    PRIMARY KEY ("uid")
);
CREATE INDEX "functional_block_link_parent" ON "functional_block_link" ("parent");
CREATE INDEX "functional_block_link_child_a" ON "functional_block_link" ("child_a");
CREATE INDEX "functional_block_link_child_b" ON "functional_block_link" ("child_b");
CREATE INDEX "functional_block_link_condition_index" ON "functional_block_link" ("condition");

CREATE OR REPLACE FUNCTION functional_block_link_condition_delete_unreferenced()
   RETURNS trigger AS $$
   BEGIN
     DELETE FROM "functional_block_link_condition" WHERE NOT "uid" IN (SELECT "condition" FROM "functional_block_link");
	 RETURN NULL;
   END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE TRIGGER "functional_block_link_delete_condition"
    AFTER UPDATE OR DELETE ON "functional_block_link"
    FOR EACH STATEMENT
    EXECUTE FUNCTION functional_block_link_condition_delete_unreferenced();
