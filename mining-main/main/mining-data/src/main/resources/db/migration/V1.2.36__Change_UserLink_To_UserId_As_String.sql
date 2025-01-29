CREATE PROPERTY Annotation.createdByUserId IF NOT EXISTS STRING (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY Annotation.updatedByUserId IF NOT EXISTS STRING;
UPDATE Annotation SET createdByUserId = createdByUserLink.userId;
UPDATE Annotation SET updatedByUserId = updatedByUserLink.userId;
ALTER PROPERTY Annotation.createdByUserLink NOTNULL FALSE;
ALTER PROPERTY Annotation.createdByUserLink MANDATORY FALSE;
UPDATE Annotation REMOVE createdByUserLink;
UPDATE Annotation REMOVE updatedByUserLink;
DROP PROPERTY Annotation.createdByUserLink FORCE;
DROP PROPERTY Annotation.updatedByUserLink FORCE;

CREATE PROPERTY DataDictionaryEntry.createdByUserId IF NOT EXISTS STRING (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY DataDictionaryEntry.updatedByUserId IF NOT EXISTS STRING;
UPDATE DataDictionaryEntry SET createdByUserId = createdByUserLink.userId;
UPDATE DataDictionaryEntry SET updatedByUserId = updatedByUserLink.userId;
ALTER PROPERTY DataDictionaryEntry.createdByUserLink NOTNULL FALSE;
ALTER PROPERTY DataDictionaryEntry.createdByUserLink MANDATORY FALSE;
UPDATE DataDictionaryEntry REMOVE createdByUserLink;
UPDATE DataDictionaryEntry REMOVE updatedByUserLink;
DROP PROPERTY DataDictionaryEntry.createdByUserLink FORCE;
DROP PROPERTY DataDictionaryEntry.updatedByUserLink FORCE;

CREATE PROPERTY SqlHistory.executedByUserId IF NOT EXISTS STRING (NOTNULL, MANDATORY TRUE);
UPDATE SqlHistory SET executedByUserId = executedByUserLink.userId;
ALTER PROPERTY SqlHistory.executedByUserLink NOTNULL FALSE;
ALTER PROPERTY SqlHistory.executedByUserLink MANDATORY FALSE;
UPDATE SqlHistory REMOVE executedByUserLink;
DROP PROPERTY SqlHistory.executedByUserLink FORCE;

CREATE PROPERTY BinaryAttachment.createdByUserId IF NOT EXISTS STRING (NOTNULL, MANDATORY TRUE);
UPDATE BinaryAttachment SET createdByUserId = createdByUserLink.userId;
ALTER PROPERTY BinaryAttachment.createdByUserLink NOTNULL FALSE;
ALTER PROPERTY BinaryAttachment.createdByUserLink MANDATORY FALSE;
UPDATE BinaryAttachment REMOVE createdByUserLink;
DROP PROPERTY BinaryAttachment.createdByUserLink FORCE;