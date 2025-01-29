--################################################################################################
-- Global Saved Search for Copybook Defined DataDictionaryEntry
--################################################################################################

INSERT INTO SavedSearch SET name="Copybook Defined DD Entries", usage="miningUi.dataDictionaryTable", projectLink=(SELECT FROM Project WHERE id=0), savedSearch='columns=DataDictionaryEntry.definedLocation&columns=DataDictionaryEntry.dataElementName&columns=DataDictionaryEntry.format&columns=DataDictionaryEntry.length&columns=Module.name&columns=DataDictionaryEntry.description&columns=DataDictionaryEntry.createdByUserId&page=1&sort=in_HasDataDictionaryEntry.out.name;ASC&filter=[{"key":"definedLocation","value":["Copybook"]}]';