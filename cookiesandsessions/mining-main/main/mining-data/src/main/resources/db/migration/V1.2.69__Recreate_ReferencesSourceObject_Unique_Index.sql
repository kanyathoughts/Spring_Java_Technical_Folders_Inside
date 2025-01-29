-- required to perform UPSERT
CREATE INDEX ReferencesSourceObject_out_in_idx IF NOT EXISTS ON ReferencesSourceObject (out, in) UNIQUE;