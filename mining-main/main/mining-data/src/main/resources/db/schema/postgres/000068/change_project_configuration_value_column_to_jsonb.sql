-- Step 1: Add a new column
ALTER TABLE project_configuration ADD COLUMN value_jsonb jsonb;

-- Step 2: Update the new column with the values from the `value` column
UPDATE project_configuration SET value_jsonb = jsonb_build_object('placeholder_xml_config_key', value);

-- Step 3: Drop the old `value` column
ALTER TABLE project_configuration DROP COLUMN value;

-- Step 4: Rename the new column to `value`
ALTER TABLE project_configuration RENAME COLUMN value_jsonb TO value;
