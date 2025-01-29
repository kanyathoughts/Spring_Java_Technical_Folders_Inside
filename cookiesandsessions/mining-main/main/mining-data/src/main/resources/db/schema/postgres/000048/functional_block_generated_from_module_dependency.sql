Alter table public."functional_block_generated_from"
    ADD COLUMN "module_dependency_hash" text,
    ADD COLUMN "dependency_changed" timestamp_zoned_milli;
