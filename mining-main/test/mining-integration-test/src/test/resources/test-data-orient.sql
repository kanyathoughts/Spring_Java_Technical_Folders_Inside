###################################################################################################################################
## RESET SEQUENCES
###################################################################################################################################
DROP SEQUENCE Reference_Sequence IF EXISTS;
CREATE SEQUENCE Reference_Sequence TYPE ORDERED START 0 INCREMENT 1

###################################################################################################################################
# DELETE FROMES
###################################################################################################################################
DELETE FROM DataFlowNode;
DELETE FROM ProxyContainer;
DELETE VERTEX AstNode
DELETE VERTEX BinaryAttachment

DELETE FROM SqlHistory UNSAFE
DELETE FROM jobInfo UNSAFE
DELETE FROM ExcelSheetUndiscovered UNSAFE
DELETE FROM EntryPoint UNSAFE
DELETE FROM ReturnPoint UNSAFE
DELETE FROM HaltPoint UNSAFE
DELETE FROM FieldInfo UNSAFE;
DELETE FROM EffortSummary UNSAFE;

###################################################################################################################################
# TEST INSERT EFFORTSUMMARY
###################################################################################################################################
INSERT INTO EffortSummary SET projectId = 1, pricingSummaries = [{"totalBatchExecAsmPrograms" : 10, "totalBatchExecCobolPrograms" : 15, "totalBatchExecPgmStatements" : 20,"totalBatchExecUnknownMissingPrograms" : 1, "totalDataFiles" : 30, "totalErrors" : 1, "totalMissingDependencies" : 2,"totalModulesWithErrors" : 35, "totalScreens" : 50, "totalSQLStatements" : 15 }, {"totalBatchExecAsmPrograms" : 20, "totalBatchExecCobolPrograms" : 10,"totalBatchExecPgmStatements" : 2, "totalBatchExecUnknownMissingPrograms" : 4, "totalDataFiles" : 24, "totalErrors" : 26, "totalMissingDependencies" : 11, "totalModulesWithErrors" : 14, "totalScreens" : 2, "totalSQLStatements" : 22 }], typeSummaries = [{"complexCount" : 13, "count" : 10, "easyCount" : 50, "errorCount" : 20, "loc" : 1000, "locComment" : 100,"technology" : "COBOL", "type" : "PROGRAM", "unmaintainableCount" : 10, "veryComplexCount" : 16}];
