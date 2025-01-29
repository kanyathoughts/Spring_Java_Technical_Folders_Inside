CREATE CLASS ContainsModule IF NOT EXISTS EXTENDS E;
CREATE PROPERTY ContainsModule.id IF NOT EXISTS LONG (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY ContainsModule.location IF NOT EXISTS LINK ModuleLocation;
CREATE PROPERTY ContainsModule.out IF NOT EXISTS LINK Module (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY ContainsModule.in IF NOT EXISTS LINK Module (NOTNULL, MANDATORY TRUE);
CREATE INDEX ContainsModule_id_idx IF NOT EXISTS ON ContainsModule (id) UNIQUE;
CREATE PROPERTY Module.out_ContainsModule IF NOT EXISTS LINKLIST ContainsModule;
CREATE PROPERTY Module.in_ContainsModule IF NOT EXISTS LINKLIST ContainsModule;
CREATE INDEX ContainsModule_in_out_idx IF NOT EXISTS ON ContainsModule (in, out) NOTUNIQUE;
CREATE INDEX ContainsModule_out_idx IF NOT EXISTS ON ContainsModule (out) NOTUNIQUE;
CREATE SEQUENCE ContainsModule_Sequence IF NOT EXISTS TYPE ORDERED START 0 INCREMENT 1;

UPDATE Module SET path=null where (path != null and objectTypeLink.storageLink.name = "FILE_SECTION");

/* This index allows the insertion of multiple null values for paths, but prevents the insertion of non-null duplicate paths. */
/* Range queries do not use this index, so that `SELECT * FROM Modules WHERE projectLink = ?` will return all rows even if their paths are null. */

DROP INDEX Module_path_idx IF EXISTS;
CREATE INDEX Module_path_idx IF NOT EXISTS ON Module (projectLink, path) UNIQUE_HASH_INDEX METADATA {ignoreNullValues: true};