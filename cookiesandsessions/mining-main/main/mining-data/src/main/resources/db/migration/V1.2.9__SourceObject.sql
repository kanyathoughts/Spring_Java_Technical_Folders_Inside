--################################################################################################
-- Source Object
--################################################################################################
CREATE CLASS SourceObject EXTENDS SourceAttachment;
CREATE PROPERTY SourceObject.projectLink LINK Project (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY SourceObject.path STRING (NOTNULL, MANDATORY TRUE);
CREATE INDEX SourceObject_path_idx ON SourceObject (projectLink, path) UNIQUE;
