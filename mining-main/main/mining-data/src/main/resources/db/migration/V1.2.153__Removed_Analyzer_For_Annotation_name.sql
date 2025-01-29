-- Removed the default analyzer
DROP INDEX Annotation_name_ft IF EXISTS;
CREATE INDEX Annotation_name_ft IF NOT EXISTS ON Annotation(name) FULLTEXT ENGINE LUCENE METADATA { "allowLeadingWildcard": true };