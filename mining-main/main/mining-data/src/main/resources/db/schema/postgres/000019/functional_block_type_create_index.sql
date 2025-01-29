CREATE INDEX "functional_block_type" ON "functional_block" USING GIN (("flags" -> 'TYPE'));
