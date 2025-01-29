CREATE TABLE "functional_block_children_deep" (
	"parent" uuid REFERENCES "functional_block" ON DELETE CASCADE,
	"child" uuid REFERENCES "functional_block" ON DELETE CASCADE,
	PRIMARY KEY ("parent", "child")
);
CREATE INDEX "functional_block_parent_deep" ON "functional_block_children_deep" ("parent");
CREATE INDEX "functional_block_child_deep" ON "functional_block_children_deep" ("child");