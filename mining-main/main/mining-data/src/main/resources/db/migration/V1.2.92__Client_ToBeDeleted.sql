-- create property toBeDeleted in Client
CREATE PROPERTY Client.toBeDeleted IF NOT EXISTS BOOLEAN (NOTNULL, DEFAULT FALSE);

-- update toBeDeleted property for existing Clients
Update Client SET toBeDeleted = FALSE;