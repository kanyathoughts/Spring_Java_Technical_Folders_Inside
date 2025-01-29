--################################################################################################
-- Effort Summary
--################################################################################################
 
CREATE CLASS TypeSummary;
CREATE PROPERTY TypeSummary.technology STRING (NOTNULL, MANDATORY TRUE); -- # need to store as STRING instead of TechnologyEnum
CREATE PROPERTY TypeSummary.type STRING (NOTNULL, MANDATORY TRUE); -- # need to store as STRING instead of TypeEnum
CREATE PROPERTY TypeSummary.count LONG (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY TypeSummary.loc LONG (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY TypeSummary.locComment LONG (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY TypeSummary.errorCount LONG (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY TypeSummary.easyCount LONG (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY TypeSummary.complexCount LONG (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY TypeSummary.veryComplexCount LONG (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY TypeSummary.unmaintainableCount LONG (NOTNULL, MANDATORY TRUE);
ALTER CLASS TypeSummary STRICTMODE TRUE;
 
CREATE CLASS PricingSummary;
CREATE PROPERTY PricingSummary.totalScreens LONG (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY PricingSummary.totalErrors LONG (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY PricingSummary.totalModulesWithErrors LONG (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY PricingSummary.totalDataFiles LONG (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY PricingSummary.totalSQLStatements LONG (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY PricingSummary.totalBatchExecPgmStatements LONG (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY PricingSummary.totalBatchExecCobolPrograms LONG (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY PricingSummary.totalBatchExecAsmPrograms LONG (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY PricingSummary.totalBatchExecUnknownMissingPrograms LONG (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY PricingSummary.totalMissingDependencies LONG (NOTNULL, MANDATORY TRUE);
ALTER CLASS PricingSummary STRICTMODE TRUE;
 
CREATE CLASS EffortSummary EXTENDS V;
CREATE PROPERTY EffortSummary.projectLink LINK Project (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY EffortSummary.typeSummaries EMBEDDEDLIST TypeSummary;
CREATE PROPERTY EffortSummary.pricingSummaries EMBEDDEDLIST PricingSummary;
ALTER CLASS EffortSummary STRICTMODE TRUE;
CREATE INDEX EffortSummary_projectLink_idx ON EffortSummary (projectLink) UNIQUE;