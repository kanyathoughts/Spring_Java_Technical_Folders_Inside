--################################################################################################
-- WMIN-6339 : Add Adavanced data properties to  DataDictionaryEntry.
--################################################################################################
CREATE PROPERTY DataDictionaryEntry.fieldLevel IF NOT EXISTS LONG (MANDATORY FALSE);
CREATE PROPERTY DataDictionaryEntry.parentGroup IF NOT EXISTS STRING (MANDATORY FALSE);
CREATE PROPERTY DataDictionaryEntry.groupPath IF NOT EXISTS STRING (MANDATORY FALSE);
CREATE PROPERTY DataDictionaryEntry.indentation IF NOT EXISTS LONG (MANDATORY FALSE);