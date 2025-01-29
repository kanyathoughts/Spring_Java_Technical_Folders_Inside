DELETE EDGE ContainsModule;
ALTER PROPERTY Module.in_ContainsModule NOTNULL FALSE;
ALTER PROPERTY Module.in_ContainsModule MANDATORY FALSE;
UPDATE Module REMOVE in_ContainsModule;
DROP PROPERTY Module.in_ContainsModule IF EXISTS FORCE;
CREATE PROPERTY Module.in_ContainsModule IF NOT EXISTS LINK ContainsModule;