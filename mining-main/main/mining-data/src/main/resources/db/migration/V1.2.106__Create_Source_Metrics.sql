-- Sequence
CREATE SEQUENCE SourceMetrics_Sequence IF NOT EXISTS TYPE ORDERED START 0 INCREMENT 1;
-- SourceMetrics
CREATE CLASS SourceMetrics IF NOT EXISTS EXTENDS AdditionalInfo;
CREATE PROPERTY SourceMetrics.id IF NOT EXISTS LONG (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY SourceMetrics.physicalLines IF NOT EXISTS INTEGER;
CREATE PROPERTY SourceMetrics.codeLines IF NOT EXISTS INTEGER;
CREATE PROPERTY SourceMetrics.commentLines IF NOT EXISTS INTEGER;
CREATE PROPERTY SourceMetrics.complexityMcCabe IF NOT EXISTS INTEGER;
CREATE PROPERTY SourceMetrics.deadCodeLines IF NOT EXISTS INTEGER (NOTNULL, MANDATORY TRUE);
ALTER PROPERTY SourceMetrics.deadCodeLines DEFAULT -1;
ALTER PROPERTY SourceMetrics.id default "sequence('SourceMetrics_Sequence').next()";