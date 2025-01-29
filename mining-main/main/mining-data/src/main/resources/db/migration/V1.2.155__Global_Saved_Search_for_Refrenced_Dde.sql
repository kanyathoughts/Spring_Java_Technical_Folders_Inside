--################################################################################################
-- Global Saved Search for Not Referenced DataDictionaryEntry
--################################################################################################

INSERT INTO SavedSearch SET name="Not Referenced DD entries", usage="miningUi.dataDictionaryTable", projectLink=(SELECT FROM Project WHERE id=0), savedSearch='columns=DataDictionaryEntry.isReferenced&columns=DataDictionaryEntry.dataElementName&columns=DataDictionaryEntry.format&columns=DataDictionaryEntry.length&columns=Module.name&columns=DataDictionaryEntry.description&columns=DataDictionaryEntry.createdByUserId&page=1&sort=in_HasDataDictionaryEntry.out.name;ASC&filter=[{"key":"isReferenced","value":["false"]}]';