DROP INDEX Module_path_idx IF EXISTS;
CREATE INDEX Module_path_idx IF NOT EXISTS ON Module (projectLink, path) NOTUNIQUE;