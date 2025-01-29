-- Discovery Project.SourceCodeRevision

CREATE PROPERTY Project.sourceCodeRevision LONG (NOTNULL, MANDATORY TRUE);
UPDATE Project SET sourceCodeRevision=1;
ALTER property Project.sourceCodeRevision DEFAULT 1;

-- Discovery SourceObject.metaDataRevision
-- Discovery SourceObject.contentRevision

CREATE PROPERTY SourceObject.metaDataRevision LONG (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY SourceObject.contentRevision LONG (NOTNULL, MANDATORY TRUE);
UPDATE SourceObject SET metaDataRevision=1;
UPDATE SourceObject SET contentRevision=1;
ALTER property SourceObject.metaDataRevision DEFAULT 1;
ALTER property SourceObject.contentRevision DEFAULT 1;

-- Discovery SourceObject.id and Sequence

CREATE PROPERTY SourceObject.id LONG (NOTNULL, MANDATORY TRUE);
CREATE SEQUENCE SourceObject_Sequence TYPE ORDERED START 0 INCREMENT 1;
UPDATE SourceObject SET id=sequence('SourceObject_Sequence').next();
CREATE INDEX SourceObject_id_idx ON SourceObject (id) UNIQUE;
