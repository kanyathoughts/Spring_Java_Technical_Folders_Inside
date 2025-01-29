--################################################################################################
-- WMIN-4885 : Add multiple properties to  DataDictionaryEntry.
--################################################################################################
CREATE PROPERTY DataDictionaryEntry.fieldFormatSize IF NOT EXISTS STRING (MANDATORY FALSE);
CREATE PROPERTY DataDictionaryEntry.picClause IF NOT EXISTS STRING (MANDATORY FALSE);
CREATE PROPERTY DataDictionaryEntry.definedLocation IF NOT EXISTS STRING (MANDATORY FALSE);
CREATE PROPERTY DataDictionaryEntry.stateLink LINK AnnotationStateEnum (MANDATORY FALSE);
CREATE PROPERTY DataDictionaryEntry.isBusiness IF NOT EXISTS BOOLEAN (MANDATORY FALSE, DEFAULT FALSE);
CREATE PROPERTY DataDictionaryEntry.fieldTransformation IF NOT EXISTS STRING (MANDATORY FALSE);
CREATE PROPERTY DataDictionaryEntry.sourceInput IF NOT EXISTS STRING (MANDATORY FALSE);
CREATE PROPERTY DataDictionaryEntry.targetOutput IF NOT EXISTS STRING (MANDATORY FALSE);
CREATE PROPERTY DataDictionaryEntry.isReferenced IF NOT EXISTS BOOLEAN (MANDATORY FALSE, DEFAULT FALSE);

--################################################################################################
-- Rename AnnotationStateEnum to WorkingStateEnum
--################################################################################################
DROP INDEX AnnotationStateEnum_name_idx;
ALTER CLASS AnnotationStateEnum NAME WorkingStateEnum;
CREATE INDEX WorkingStateEnum_name_idx ON WorkingStateEnum (name) UNIQUE;