DROP INDEX ContainsModule_in_out_idx IF EXISTS;
CREATE INDEX ContainsModule_in_out_idx IF NOT EXISTS ON ContainsModule (in, out) UNIQUE;
