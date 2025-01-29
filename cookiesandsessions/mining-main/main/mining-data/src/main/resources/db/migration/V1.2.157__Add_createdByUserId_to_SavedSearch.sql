--################################################################################################
-- Add property createdByUserId to Saved Search
--################################################################################################

CREATE PROPERTY SavedSearch.createdByUserId IF NOT EXISTS STRING;

--################################################################################################
-- Replace the unique index (name, usage, projectLink) with index (name, usage, projectLink, 
-- createdByUserId)
--################################################################################################

DROP INDEX SavedSearch_idx IF EXISTS;
CREATE INDEX SavedSearch_idx ON SavedSearch (name, usage, projectLink, createdByUserId) UNIQUE;

--################################################################################################
-- Drop the unique index (name, projectLink)
--################################################################################################

DROP INDEX SavedSearch_name_idx IF EXISTS;

--################################################################################################
-- Replace the not unique index (usage, projectLink) with index (usage, projectLink, 
-- createdByUserId) 
--################################################################################################

DROP INDEX SavedSearch_usage_idx IF EXISTS;
CREATE INDEX SavedSearch_usage_idx ON SavedSearch (usage, projectLink, createdByUserId) NOTUNIQUE;