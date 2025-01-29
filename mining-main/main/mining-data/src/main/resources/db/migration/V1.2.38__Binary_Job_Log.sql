-- drop the old log property - this will also discard old logs
ALTER PROPERTY jobLog.log NOTNULL FALSE;
ALTER PROPERTY jobLog.log MANDATORY FALSE;
UPDATE jobLog REMOVE log;
DROP PROPERTY jobLog.log FORCE;

-- recreate property as binary
CREATE PROPERTY jobLog.log BINARY (NOTNULL, MANDATORY TRUE);