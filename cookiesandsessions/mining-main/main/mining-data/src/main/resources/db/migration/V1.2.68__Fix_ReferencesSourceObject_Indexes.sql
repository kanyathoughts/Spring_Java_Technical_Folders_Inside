-- combined index on (in, out) - the database didn't actually use it :-(
DROP INDEX ReferencesSourceObject_out_in_idx IF EXISTS;

-- create separate indexes on in and out
CREATE INDEX ReferencesSourceObject_in_idx IF NOT EXISTS ON ReferencesSourceObject (in) NOTUNIQUE;
CREATE INDEX ReferencesSourceObject_out_idx IF NOT EXISTS ON ReferencesSourceObject (out) NOTUNIQUE;