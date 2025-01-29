CREATE INDEX Calls_in_idx IF NOT EXISTS ON Calls (in) NOTUNIQUE;
CREATE INDEX HasAnnotation_in_idx IF NOT EXISTS ON HasAnnotation (in) NOTUNIQUE;
CREATE INDEX HasDataDictionaryEntry_in_idx IF NOT EXISTS ON HasDataDictionaryEntry (in) NOTUNIQUE;
CREATE INDEX HasTaxonomy_in_idx IF NOT EXISTS ON HasTaxonomy (in) NOTUNIQUE;
CREATE INDEX Includes_in_idx IF NOT EXISTS ON Includes (in) NOTUNIQUE;
CREATE INDEX None_in_idx IF NOT EXISTS ON None (in) NOTUNIQUE;
CREATE INDEX ReadsWrites_in_idx IF NOT EXISTS ON ReadsWrites (in) NOTUNIQUE;
CREATE INDEX References_in_idx IF NOT EXISTS ON References (in) NOTUNIQUE;