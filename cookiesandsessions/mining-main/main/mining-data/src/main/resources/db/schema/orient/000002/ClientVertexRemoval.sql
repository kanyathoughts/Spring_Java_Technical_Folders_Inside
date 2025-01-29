--################################################################################################
-- Project
--################################################################################################
CREATE PROPERTY Project.clientId IF NOT EXISTS LONG;
UPDATE Project SET clientId = clientLink.id WHERE clientId is Null;
DROP INDEX Project_name_idx IF EXISTS;
DROP PROPERTY Project.clientLink IF EXISTS;
UPDATE Project REMOVE clientLink;
ALTER PROPERTY Project.clientId MANDATORY TRUE;
ALTER PROPERTY Project.clientId NOTNULL TRUE;
CREATE INDEX Project_name_idx IF NOT EXISTS ON Project (name, clientId) UNIQUE;

--################################################################################################
-- SavedSearch
--################################################################################################
DROP INDEX SavedSearch_idx IF EXISTS;
DROP INDEX SavedSearch_usage_idx IF EXISTS;
CREATE PROPERTY SavedSearch.clientId IF NOT EXISTS LONG;
UPDATE SavedSearch SET clientId = clientLink.id WHERE clientId is Null;
UPDATE SavedSearch REMOVE clientLink;
DROP PROPERTY SavedSearch.clientLink IF EXISTS;
CREATE INDEX SavedSearch_idx IF NOT EXISTS ON SavedSearch (name, usage, scope, projectLink, clientId, createdByUserId) UNIQUE
CREATE INDEX SavedSearch_usage_idx IF NOT EXISTS ON SavedSearch (usage, scope, projectLink, clientId, createdByUserId) NOTUNIQUE;
