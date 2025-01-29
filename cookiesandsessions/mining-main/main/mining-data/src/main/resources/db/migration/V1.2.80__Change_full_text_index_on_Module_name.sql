-- The old index was using the EnglishAnalyzer which treated numbers and dots in a way that was not consistent with the expectations of our search UI.
DROP INDEX Module_name_ft IF EXISTS;
CREATE INDEX Module_name_ft ON Module(name) FULLTEXT ENGINE LUCENE METADATA { "allowLeadingWildcard": true, "default": "innowake.mining.data.plugin.lucene.LowercasingKeywordAnalyzer" };
