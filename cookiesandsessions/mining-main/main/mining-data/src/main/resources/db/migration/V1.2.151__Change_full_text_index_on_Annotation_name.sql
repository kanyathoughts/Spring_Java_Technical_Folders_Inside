-- The old index was not allowing wild card entries.
DROP INDEX Annotation_name_ft IF EXISTS;
CREATE INDEX Annotation_name_ft IF NOT EXISTS ON Annotation(name) FULLTEXT ENGINE LUCENE METADATA { "allowLeadingWildcard": true, "default": "org.apache.lucene.analysis.en.EnglishAnalyzer" };