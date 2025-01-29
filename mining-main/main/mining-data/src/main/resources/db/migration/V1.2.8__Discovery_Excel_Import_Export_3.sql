--################################################################################################
-- Schema extension required by Discovery Minification. 
--################################################################################################

-- store original type from Modules sheet since it differs from Module.type
CREATE PROPERTY ExcelSheetModules.type STRING;