CREATE INDEX Statement_projectLink_idx IF NOT EXISTS ON Statement (projectLink) NOTUNIQUE;
CREATE INDEX Statement_moduleLink_idx IF NOT EXISTS ON Statement (moduleLink) NOTUNIQUE;