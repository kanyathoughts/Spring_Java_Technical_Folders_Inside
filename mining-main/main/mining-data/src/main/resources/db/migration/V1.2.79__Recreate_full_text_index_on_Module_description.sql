-- The Module_description full text index is used by ModuleDao.findByDescription.
-- It is recreated here in the case that it was erroneously deleted in the previous migration.
CREATE INDEX Module_description_ft IF NOT EXISTS ON Module(description) FULLTEXT ENGINE LUCENE METADATA {"default" : "org.apache.lucene.analysis.en.EnglishAnalyzer"};
