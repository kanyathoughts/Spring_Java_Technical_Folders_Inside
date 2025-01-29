--################################################################################################
-- Job log Object
--################################################################################################
CREATE CLASS jobLog;
CREATE PROPERTY jobLog.jobId STRING (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY jobLog.log STRING (NOTNULL, MANDATORY TRUE);
CREATE INDEX jobLog_id_idx ON jobLog (jobId) UNIQUE;