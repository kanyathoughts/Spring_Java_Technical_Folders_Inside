DROP INDEX Module_path_idx;
CREATE INDEX Module_path_idx ON Module (projectLink, path) NOTUNIQUE;