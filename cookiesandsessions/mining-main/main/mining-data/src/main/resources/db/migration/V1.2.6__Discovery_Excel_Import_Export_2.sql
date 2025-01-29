--################################################################################################
-- Schema extension required by Discovery Minification. 
--################################################################################################

CREATE PROPERTY ExcelSheetDependencies.attributes STRING;

CREATE CLASS ExcelSheetConditionalOutlines;
CREATE PROPERTY ExcelSheetConditionalOutlines.projectLink LINK Project (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY ExcelSheetConditionalOutlines.moduleLink LINK Module (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY ExcelSheetConditionalOutlines.type STRING;
CREATE PROPERTY ExcelSheetConditionalOutlines.relevance STRING;
CREATE PROPERTY ExcelSheetConditionalOutlines.line INTEGER;
CREATE PROPERTY ExcelSheetConditionalOutlines.expression STRING;
CREATE INDEX ExcelSheetConditionalOutlines_projectLink_idx ON ExcelSheetConditionalOutlines (projectLink) NOTUNIQUE;
CREATE INDEX ExcelSheetConditionalOutlines_moduleLink_idx ON ExcelSheetConditionalOutlines (moduleLink) NOTUNIQUE;