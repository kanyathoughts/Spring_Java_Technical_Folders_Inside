-- additional sequence for module UIDs
CREATE SEQUENCE ModuleUid_Sequence TYPE ORDERED START 0 INCREMENT 1;

-- additional properties to hold a module UID and the properties from the ExcelSheetModules class
CREATE PROPERTY Module.uid IF NOT EXISTS LONG (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY Module.statements IF NOT EXISTS INTEGER;
CREATE PROPERTY Module.sqlStatements IF NOT EXISTS INTEGER;
CREATE PROPERTY Module.representation IF NOT EXISTS STRING;
CREATE PROPERTY Module.errors IF NOT EXISTS INTEGER;
CREATE PROPERTY Module.excelType IF NOT EXISTS STRING;

-- set UID of all existing records to -1. the new implementation will not work on existing data anyways
UPDATE Module SET uid=-1;

CREATE INDEX Module_uid_idx IF NOT EXISTS ON Module (uid) NOTUNIQUE;