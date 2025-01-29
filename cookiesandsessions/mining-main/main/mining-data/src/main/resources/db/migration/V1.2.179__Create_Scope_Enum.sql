--################################################################################################
-- Sequence
--################################################################################################

CREATE SEQUENCE SavedSearch_Sequence IF NOT EXISTS TYPE ORDERED START 0 INCREMENT 1;

--################################################################################################
-- Create the ScopeEnum class
--################################################################################################

CREATE CLASS ScopeEnum IF NOT EXISTS EXTENDS V;
CREATE PROPERTY ScopeEnum.name IF NOT EXISTS STRING (NOTNULL, MANDATORY TRUE);
INSERT INTO ScopeEnum SET name="INDIVIDUAL";
INSERT INTO ScopeEnum SET name="PROJECT";
INSERT INTO ScopeEnum SET name="CLIENT";
INSERT INTO ScopeEnum SET name="GLOBAL";

--################################################################################################
-- Add property id, scope to Saved Search
--################################################################################################

CREATE PROPERTY SavedSearch.id IF NOT EXISTS LONG (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY SavedSearch.scope IF NOT EXISTS LINK ScopeEnum (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY SavedSearch.clientLink IF NOT EXISTS LINK Client;
ALTER PROPERTY SavedSearch.projectLink NOTNULL FALSE;
ALTER PROPERTY SavedSearch.projectLink MANDATORY FALSE;
ALTER PROPERTY SavedSearch.id default "sequence('SavedSearch_Sequence').next()";

--################################################################################################
-- Update id, scope for existing Saved Search
--################################################################################################

UPDATE SavedSearch SET id = sequence('SavedSearch_Sequence').next(), scope = (SELECT FROM ScopeEnum where name = 'GLOBAL') where projectLink.id = 0;
UPDATE SavedSearch SET id = sequence('SavedSearch_Sequence').next(), scope = (SELECT FROM ScopeEnum where name = 'INDIVIDUAL') where projectLink.id != 0;

--################################################################################################
-- Create the unique index (name)
--################################################################################################

CREATE INDEX ScopeEnum_name_IDX IF NOT EXISTS ON ScopeEnum (name) UNIQUE;

--################################################################################################
-- Replace the unique index (name, usage, projectLink, createdByUserId) with index (name, usage, 
-- scope, projectLink, clientLink, createdByUserId)
--################################################################################################

DROP INDEX SavedSearch_idx IF EXISTS;
CREATE INDEX SavedSearch_idx ON SavedSearch (name, usage, scope, projectLink, clientLink, createdByUserId) UNIQUE;

--################################################################################################
-- Create the unique index (id)
--################################################################################################

DROP INDEX SavedSearch_id_idx IF EXISTS;
CREATE INDEX SavedSearch_id_idx ON SavedSearch (id) UNIQUE;

--################################################################################################
-- Replace the not unique index (usage, projectLink, createdByUserId) with index (usage, scope,
-- projectLink, clientLink, createdByUserId) 
--################################################################################################

DROP INDEX SavedSearch_usage_idx IF EXISTS;
CREATE INDEX SavedSearch_usage_idx ON SavedSearch (usage, scope, projectLink, clientLink, createdByUserId) NOTUNIQUE;
