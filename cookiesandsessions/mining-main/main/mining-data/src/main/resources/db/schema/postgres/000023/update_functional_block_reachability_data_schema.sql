ALTER TABLE public."functional_block_reachability_data"
    DROP COLUMN access_type,
    ADD COLUMN access_type text[];
