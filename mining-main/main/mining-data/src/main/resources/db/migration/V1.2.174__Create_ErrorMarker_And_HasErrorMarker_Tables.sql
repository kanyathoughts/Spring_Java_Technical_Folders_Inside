-- THIS MIGRATION HAS BEEN REVERTED

-- Instead, we only create the moduleLocation on ExcelSheetErrors here now

CREATE PROPERTY ExcelSheetErrors.moduleLocation IF NOT EXISTS EMBEDDED ModuleLocation;
ALTER PROPERTY ExcelSheetErrors.moduleLink NOTNULL FALSE;
ALTER PROPERTY ExcelSheetErrors.moduleLink MANDATORY FALSE;
CREATE PROPERTY Module.errorMarkerLinks IF NOT EXISTS LINKLIST ExcelSheetErrors (NOTNULL, MANDATORY FALSE, DEFAULT []);


-- PREVIOUS CONTENT:

-- Creates ErrorMarker Table
--CREATE CLASS ErrorMarker IF NOT EXISTS EXTENDS V;
--CREATE PROPERTY ErrorMarker.cause IF NOT EXISTS STRING (NOTNULL);
--CREATE PROPERTY ErrorMarker.key IF NOT EXISTS STRING (NOTNULL);
--CREATE PROPERTY ErrorMarker.moduleLocation IF NOT EXISTS EMBEDDED ModuleLocation;
--CREATE PROPERTY ErrorMarker.projectLink IF NOT EXISTS LINK Project (NOTNULL);
--CREATE PROPERTY ErrorMarker.severity IF NOT EXISTS STRING (NOTNULL);
--
---- Creates HasErrorMarker Edge
--CREATE CLASS HasErrorMarker IF NOT EXISTS EXTENDS E;
--CREATE PROPERTY HasErrorMarker.out IF NOT EXISTS LINK Module (NOTNULL, MANDATORY TRUE);
--CREATE PROPERTY HasErrorMarker.in IF NOT EXISTS LINK ErrorMarker (NOTNULL, MANDATORY TRUE);
--
--CREATE PROPERTY ErrorMarker.in_HasErrorMarker IF NOT EXISTS LINK HasErrorMarker;
--
--CREATE PROPERTY Module.out_HasErrorMarker IF NOT EXISTS LINKLIST HasErrorMarker;
--
--ALTER CLASS HasErrorMarker STRICTMODE TRUE;
--
--CREATE INDEX ErrorMarker_projectLink_idx IF NOT EXISTS ON ErrorMarker (projectLink) NOTUNIQUE;
--CREATE INDEX ErrorMarker_in_HasErrorMarker_idx IF NOT EXISTS ON ErrorMarker (in_HasErrorMarker) NOTUNIQUE;
--CREATE INDEX ErrorMarker_projectLink_in_HasErrorMarker_idx IF NOT EXISTS ON ErrorMarker (projectLink, in_HasErrorMarker) NOTUNIQUE;
--
--CREATE INDEX HasErrorMarker_in_idx IF NOT EXISTS ON HasErrorMarker (in) NOTUNIQUE;
--CREATE INDEX HasErrorMarker_out_idx IF NOT EXISTS ON HasErrorMarker (out) NOTUNIQUE;
--CREATE INDEX HasErrorMarker_in_out_idx IF NOT EXISTS ON HasErrorMarker (in, out) NOTUNIQUE;