--################################################################################################
-- Source Object
--################################################################################################
CREATE CLASS ReferencesSourceObject EXTENDS E;
CREATE PROPERTY SourceObject.out_ReferencesSourceObject LINKLIST ReferencesSourceObject;
CREATE PROPERTY SourceObject.in_ReferencesSourceObject LINKLIST ReferencesSourceObject;
CREATE PROPERTY ReferencesSourceObject.out LINK SourceObject (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY ReferencesSourceObject.in LINK SourceObject (NOTNULL, MANDATORY TRUE);
CREATE INDEX ReferencesSourceObject_out_in_idx ON ReferencesSourceObject (out, in) UNIQUE;