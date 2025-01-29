--################################################################################################
-- Job log Object
--################################################################################################
CREATE CLASS JobLog;
CREATE PROPERTY JobLog.jobId STRING (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY JobLog.log BINARY (NOTNULL, MANDATORY TRUE);
CREATE INDEX JobLog_id_idx ON jobLog (jobId) UNIQUE;