CREATE TABLE "functional_block_reachability_data" (
    "uid" uuid,
    "functional_block" uuid NOT NULL REFERENCES "functional_block" ON DELETE CASCADE,
    "upper_bound_module" uuid REFERENCES "module" ON DELETE SET NULL,
    "lower_bound_module" uuid REFERENCES "module" ON DELETE SET NULL,
    "access_module" uuid REFERENCES "module" ON DELETE SET NULL,
    "access_type" text,
    PRIMARY KEY ("uid")
);
CREATE INDEX "functional_block_reachability_data_upper_bound" ON "functional_block_reachability_data" ("upper_bound_module");
CREATE INDEX "functional_block_reachability_data_lower_bound" ON "functional_block_reachability_data" ("lower_bound_module");
CREATE INDEX "functional_block_reachability_data_access_module" ON "functional_block_reachability_data" ("access_module");
CREATE INDEX "functional_block_reachability_data_upper_and_lower_bound" ON "functional_block_reachability_data" ("upper_bound_module", "lower_bound_module");