-- Datetime field in Project & Module for last scan date
CREATE PROPERTY Project.metricsDate IF NOT EXISTS Datetime (MANDATORY FALSE);
CREATE PROPERTY Module.modifiedDate IF NOT EXISTS Datetime (NOTNULL, MANDATORY TRUE);

ALTER PROPERTY Module.modifiedDate DEFAULT "SYSDATE()";

UPDATE Module SET modifiedDate = SYSDATE(); 