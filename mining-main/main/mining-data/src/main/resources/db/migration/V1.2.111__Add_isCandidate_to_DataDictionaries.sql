--################################################################################################
-- WMIN-4526 : Add a BOOLEAN flag called isCandidate to  Data Dictionary Candidates.
--################################################################################################
CREATE PROPERTY DataDictionaryEntry.isCandidate IF NOT EXISTS BOOLEAN (NOTNULL, MANDATORY FALSE, DEFAULT FALSE);