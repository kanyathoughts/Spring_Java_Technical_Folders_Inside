CREATE TABLE "reachability_data_intermediate_modules" (
    "reachability_data" uuid NOT NULL REFERENCES "functional_block_reachability_data" ON DELETE CASCADE,
    "intermediate_module" uuid NOT NULL REFERENCES "module" ON DELETE CASCADE,
    PRIMARY KEY ("reachability_data", "intermediate_module")
);

