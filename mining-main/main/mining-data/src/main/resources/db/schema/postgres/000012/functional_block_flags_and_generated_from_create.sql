ALTER TABLE public."functional_block"
    ADD COLUMN flags jsonb;

CREATE TABLE public."functional_block_generated_from" (
	"functional_block" uuid REFERENCES "functional_block" ON DELETE CASCADE,
    "module_link_hash" text,
    "module_content_hash" text,
    "annotation_nid" bigint,
    PRIMARY KEY ("functional_block")
);
CREATE INDEX "functional_block_generated_from_functional_block" ON "functional_block_generated_from" ("functional_block");