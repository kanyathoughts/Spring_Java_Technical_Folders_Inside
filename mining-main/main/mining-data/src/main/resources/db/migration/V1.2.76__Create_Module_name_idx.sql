-- Ensure matching by name and path is case-insensitive
ALTER PROPERTY Module.name COLLATE ci;
ALTER PROPERTY Module.path COLLATE ci;

-- Index for finding Module by name and project
CREATE INDEX Module_name_idx IF NOT EXISTS ON Module (projectLink, name) NOTUNIQUE;
