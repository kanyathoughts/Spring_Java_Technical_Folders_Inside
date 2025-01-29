--################################################################################################
-- Schema extension required by Discovery Minification. 
--################################################################################################

CREATE CLASS ExcelSheetModules;
CREATE PROPERTY ExcelSheetModules.projectLink LINK Project (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY ExcelSheetModules.moduleLink LINK Module (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY ExcelSheetModules.representation STRING;
CREATE PROPERTY ExcelSheetModules.errors INTEGER;
CREATE PROPERTY ExcelSheetModules.statements INTEGER;
CREATE PROPERTY ExcelSheetModules.sqlStatements INTEGER;
CREATE INDEX ExcelSheetModules_projectLink_idx ON ExcelSheetModules (projectLink) NOTUNIQUE;
CREATE INDEX ExcelSheetModules_moduleLink_idx ON ExcelSheetModules (moduleLink) NOTUNIQUE;

CREATE CLASS ExcelSheetStatements;
CREATE PROPERTY ExcelSheetStatements.projectLink LINK Project (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY ExcelSheetStatements.moduleLink LINK Module (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY ExcelSheetStatements.statement STRING;
CREATE PROPERTY ExcelSheetStatements.string STRING;
CREATE INDEX ExcelSheetStatements_projectLink_idx ON ExcelSheetStatements (projectLink) NOTUNIQUE;
CREATE INDEX ExcelSheetStatements_moduleLink_idx ON ExcelSheetStatements (moduleLink) NOTUNIQUE;

CREATE CLASS ExcelSheetSql;
CREATE PROPERTY ExcelSheetSql.projectLink LINK Project (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY ExcelSheetSql.moduleLink LINK Module (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY ExcelSheetSql.statement STRING;
CREATE PROPERTY ExcelSheetSql.sqlLength INTEGER;
CREATE PROPERTY ExcelSheetSql.tables INTEGER;
CREATE PROPERTY ExcelSheetSql.distinctTables INTEGER;
CREATE PROPERTY ExcelSheetSql.customComplexity INTEGER;
CREATE PROPERTY ExcelSheetSql.halsteadComplexity FLOAT;
CREATE PROPERTY ExcelSheetSql.halsteadDifficulty FLOAT;
CREATE PROPERTY ExcelSheetSql.string STRING;
CREATE INDEX ExcelSheetSql_projectLink_idx ON ExcelSheetSql (projectLink) NOTUNIQUE;
CREATE INDEX ExcelSheetSql_moduleLink_idx ON ExcelSheetSql (moduleLink) NOTUNIQUE;

CREATE CLASS ExcelSheetErrors;
CREATE PROPERTY ExcelSheetErrors.projectLink LINK Project (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY ExcelSheetErrors.moduleLink LINK Module (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY ExcelSheetErrors.severity STRING;
CREATE PROPERTY ExcelSheetErrors.line INTEGER;
CREATE PROPERTY ExcelSheetErrors.key STRING;
CREATE PROPERTY ExcelSheetErrors.cause STRING;
CREATE INDEX ExcelSheetErrors_projectLink_idx ON ExcelSheetErrors (projectLink) NOTUNIQUE;
CREATE INDEX ExcelSheetErrors_moduleLink_idx ON ExcelSheetErrors (moduleLink) NOTUNIQUE;

CREATE CLASS ExcelSheetDeadCode;
CREATE PROPERTY ExcelSheetDeadCode.projectLink LINK Project (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY ExcelSheetDeadCode.moduleLink LINK Module (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY ExcelSheetDeadCode.deadCode STRING;
CREATE PROPERTY ExcelSheetDeadCode.startingLine INTEGER;
CREATE PROPERTY ExcelSheetDeadCode.numberOfLines INTEGER;
CREATE INDEX ExcelSheetDeadCode_projectLink_idx ON ExcelSheetDeadCode (projectLink) NOTUNIQUE;
CREATE INDEX ExcelSheetDeadCode_moduleLink_idx ON ExcelSheetDeadCode (moduleLink) NOTUNIQUE;

CREATE CLASS ExcelSheetUndiscovered;
CREATE PROPERTY ExcelSheetUndiscovered.projectLink LINK Project (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY ExcelSheetUndiscovered.name STRING;
CREATE PROPERTY ExcelSheetUndiscovered.path STRING;
CREATE INDEX ExcelSheetUndiscovered_projectLink_idx ON ExcelSheetUndiscovered (projectLink) NOTUNIQUE;

-- use EMBEDDED for additional Reference properties because we do not expect much data
CREATE CLASS ExcelSheetDependencies;
CREATE PROPERTY ExcelSheetDependencies.binding STRING;
CREATE PROPERTY Reference.excelSheetDependencies EMBEDDED ExcelSheetDependencies;
