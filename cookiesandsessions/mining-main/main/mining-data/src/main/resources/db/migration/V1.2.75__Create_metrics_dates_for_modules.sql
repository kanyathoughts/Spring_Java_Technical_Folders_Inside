-- Datetime field in Module for last scan date
CREATE PROPERTY Module.metricsDate IF NOT EXISTS Datetime (MANDATORY FALSE);

UPDATE Module SET metricsDate = projectLink.metricsDate;