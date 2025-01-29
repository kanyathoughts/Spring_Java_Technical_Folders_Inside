/* This index allows the insertion of multiple null values for paths, but prevents the insertion of non-null duplicate paths. */
/* Range queries do not use this index, so that `SELECT * FROM Modules WHERE projectLink = ?` will return all rows even if their paths are null. */

DROP INDEX Module_path_idx;
CREATE INDEX Module_path_idx ON Module (projectLink, path) UNIQUE_HASH_INDEX METADATA {ignoreNullValues: true};