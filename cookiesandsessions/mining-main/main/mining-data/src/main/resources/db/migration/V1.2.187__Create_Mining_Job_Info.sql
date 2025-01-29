CREATE CLASS MiningJobInfo IF NOT EXISTS EXTENDS V;
CREATE PROPERTY MiningJobInfo.jobId IF NOT EXISTS STRING (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY MiningJobInfo.projectLink IF NOT EXISTS LINK Project;
CREATE PROPERTY MiningJobInfo.moduleLink IF NOT EXISTS LINK ModuleUnit;

CREATE INDEX MiningJobInfo_jobId_idx IF NOT EXISTS ON MiningJobInfo(jobId) UNIQUE;
CREATE INDEX MiningJobInfo_projectLink_moduleLink_idx IF NOT EXISTS ON MiningJobInfo(projectLink, moduleLink) NOTUNIQUE;