-- Index for finding SourceObject by project and technology
CREATE INDEX SourceObject_project_technology_idx IF NOT EXISTS ON SourceObject (projectLink, technologyLink) NOTUNIQUE;
