-- Migrate the data from ExcelSheetStatements to Statement and ExcelSheetSql to SqlStatement and drop ExcelSheetStatements, ExcelSheetSql

INSERT INTO Statement SET id=Sequence('Statement_Sequence').next() FROM (SELECT projectLink, string as text, $statementType as statementTypeLink, moduleLink, moduleLink.objectTypeLink.technologyLink as technologyLink FROM ExcelSheetStatements LET $statementType=(SELECT FROM StatementTypeEnum WHERE name=$parent.current.statement.replace(' ', '_')));


INSERT INTO SqlStatement SET id=Sequence('Statement_Sequence').next() FROM (SELECT projectLink, string as text, $statementType as statementTypeLink, moduleLink, moduleLink.objectTypeLink.technologyLink as technologyLink, customComplexity, distinctTables, halsteadComplexity, halsteadDifficulty, sqlLength, tables FROM ExcelSheetSql LET $statementType=(SELECT FROM StatementTypeEnum WHERE name=$parent.current.statement.replace(' ', '_')));

DROP CLASS ExcelSheetStatements;
DROP CLASS ExcelSheetSql;
