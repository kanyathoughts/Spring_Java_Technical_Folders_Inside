-- Discovery Project.metricsVersion

CREATE PROPERTY Project.metricsVersion IF NOT EXISTS STRING (NOTNULL, MANDATORY TRUE, DEFAULT "");
-- Set default value on all existing projects
UPDATE Project SET metricsVersion="";