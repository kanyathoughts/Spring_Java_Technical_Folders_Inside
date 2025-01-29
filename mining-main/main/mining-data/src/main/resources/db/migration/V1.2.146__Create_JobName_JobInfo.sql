CREATE PROPERTY JobInfo.jobName IF NOT EXISTS STRING (NOTNULL, DEFAULT "unknown");
UPDATE JobInfo SET jobName = "unknown" WHERE jobName IS NULL;