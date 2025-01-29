-- create property toBeDeleted in Project
CREATE PROPERTY Project.toBeDeleted IF NOT EXISTS BOOLEAN (NOTNULL, DEFAULT FALSE);

-- update toBeDeleted property for existing Projects
Update Project SET toBeDeleted = FALSE;