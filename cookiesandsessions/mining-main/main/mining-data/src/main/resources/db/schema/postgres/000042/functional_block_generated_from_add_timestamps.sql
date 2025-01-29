Alter table public."functional_block_generated_from"
    ADD COLUMN "content_changed" timestamp_zoned_milli,
    ADD COLUMN "missing_since" timestamp_zoned_milli;
