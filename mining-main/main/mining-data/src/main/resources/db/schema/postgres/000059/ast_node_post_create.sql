ALTER TABLE "ast_node" ADD FOREIGN KEY ("parent") REFERENCES "ast_node" ("id") ON DELETE SET NULL DEFERRABLE;

CREATE INDEX "ast_node_parent_idx" ON "ast_node" USING btree ("parent");
CREATE INDEX "ast_node_module_idx" ON "ast_node" USING btree ("module");
CREATE INDEX "ast_node_included_module_idx" ON "ast_node" USING btree ("included_module");

-- Index to improve DataLineage
CREATE INDEX "ast_node_assembled_offset_idx" ON "ast_node" USING btree ("module", (("location")."assembled_offset"));
