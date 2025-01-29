CREATE TABLE IF NOT EXISTS "functional_block_referenced_data_dictionary" (
    "functional_block" uuid NOT NULL REFERENCES "functional_block" ("uid") ON DELETE CASCADE,
    "data_dictionary" uuid NOT NULL REFERENCES "data_dictionary" ("uid") ON DELETE CASCADE,
    PRIMARY KEY ("functional_block", "data_dictionary")
);
