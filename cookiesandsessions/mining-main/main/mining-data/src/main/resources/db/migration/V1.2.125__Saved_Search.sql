--################################################################################################
-- Saved Search
--################################################################################################

CREATE CLASS SavedSearch IF NOT EXISTS EXTENDS V;
CREATE PROPERTY SavedSearch.name STRING (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY SavedSearch.usage STRING (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY SavedSearch.projectLink LINK Project (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY SavedSearch.savedSearch STRING (NOTNULL, MANDATORY TRUE);
CREATE INDEX SavedSearch_idx ON SavedSearch (name, usage, projectLink) UNIQUE;
CREATE INDEX SavedSearch_name_idx ON SavedSearch (name, projectLink) UNIQUE;
CREATE INDEX SavedSearch_usage_idx ON SavedSearch (usage, projectLink) NOTUNIQUE;