--################################################################################################
-- Schema extension required for support of the Job API. 
--################################################################################################

CREATE CLASS jobInfo;
CREATE PROPERTY jobInfo.jobId STRING (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY jobInfo.userName STRING (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY jobInfo.jobDescription STRING;
CREATE PROPERTY jobInfo.stepDescription STRING;
CREATE PROPERTY jobInfo.status STRING;
CREATE PROPERTY jobInfo.submitTime DATETIME;
CREATE PROPERTY jobInfo.scheduledStartTime DATETIME;
CREATE PROPERTY jobInfo.startTime DATETIME;
CREATE PROPERTY jobInfo.finishTime DATETIME;
CREATE PROPERTY jobInfo.result BINARY;
CREATE PROPERTY jobInfo.pendingTasks INTEGER;
CREATE PROPERTY jobInfo.totalWorkUnits INTEGER;
CREATE PROPERTY jobInfo.processedWorkUnits DOUBLE;
CREATE PROPERTY jobInfo.messages BINARY;
CREATE INDEX jobInfo_id_idx ON jobInfo (jobId) UNIQUE;