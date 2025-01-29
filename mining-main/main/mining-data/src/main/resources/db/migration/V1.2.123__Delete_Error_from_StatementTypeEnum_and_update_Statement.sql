-- Updates all Statements that have statementType "ERROR" to "UNKNOWN"
UPDATE Statement SET statementTypeLink = (select @rid from StatementTypeEnum where name='UNKNOWN') where statementTypeLink in (select @rid from StatementTypeEnum where name='ERROR'); 

-- Removes the ERROR entry from StatementTypeEnum
DELETE VERTEX from StatementTypeEnum where name = 'ERROR'; 