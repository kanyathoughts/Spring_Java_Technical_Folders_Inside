-- Step 1: Add the node_location column
ALTER TABLE error_marker
ADD COLUMN node_location ast_node_location;

-- Step 2: Update the node_location column
UPDATE error_marker
SET node_location = CASE
        WHEN module.technology IN ('COBOL', 'NATURAL', 'ASSEMBLER', 'PL1') THEN null
        ELSE ROW(
            (location).offset,
            (location).length,
            (location).offset,
            (location).length,
            (location).offset,
            (location).length,
            null, null)::ast_node_location
    END
FROM (
    SELECT module.uid, module.technology
    FROM module
) AS module
WHERE error_marker.module = module.uid;

-- Step 3: Drop the old location column
ALTER TABLE error_marker DROP COLUMN location;

-- Step 4: Rename the node_location column to location
ALTER TABLE error_marker RENAME COLUMN node_location TO location;
