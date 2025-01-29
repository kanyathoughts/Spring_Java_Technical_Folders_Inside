CREATE INDEX "functional_block_status_index" ON "functional_block" USING GIN (("flags" -> 'STATUS'));
