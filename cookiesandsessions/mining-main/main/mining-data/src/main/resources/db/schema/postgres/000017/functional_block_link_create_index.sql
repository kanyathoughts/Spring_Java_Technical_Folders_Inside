--must use IF NOT EXISTS check as index can already be present
CREATE INDEX IF NOT EXISTS "functional_block_link_condition_index" ON "functional_block_link" ("condition");