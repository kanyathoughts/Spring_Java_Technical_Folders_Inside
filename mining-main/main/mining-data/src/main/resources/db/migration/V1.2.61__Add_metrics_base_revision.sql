-- Discovery Project.metricsBaseRevision

CREATE PROPERTY Project.metricsBaseRevision LONG (NOTNULL, MANDATORY TRUE);
UPDATE Project SET metricsBaseRevision=0; -- one less than sourceCodeRevision in migration 1.2.31
ALTER property Project.metricsBaseRevision DEFAULT 0;
