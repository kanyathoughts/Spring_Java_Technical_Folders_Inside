-- create property requiresReview in Module
CREATE PROPERTY Module.requiresReview IF NOT EXISTS BOOLEAN (NOTNULL, DEFAULT FALSE);

-- update requiresReview property for existing Modules
Update Module SET requiresReview = FALSE;