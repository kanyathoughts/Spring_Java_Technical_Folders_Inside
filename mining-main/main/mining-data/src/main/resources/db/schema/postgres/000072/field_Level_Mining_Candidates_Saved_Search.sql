INSERT INTO saved_search (name, usage, project, scope, saved_search) 
VALUES (
	'Field Level Mining Candidates', 
	'miningUi.dataDictionaryTable', 
	(SELECT uid FROM project WHERE nid = 0), 
	'GLOBAL',
	'columns=DataDictionaryEntry.dataElementName&columns=DataDictionaryEntry.isBusiness&columns=DataDictionaryEntry.accessType&columns=DataDictionaryEntry.scopeNames&columns=DataDictionaryEntry.isReferenced&columns=Module.name&sort={content_name: ASC}&filter=[{"key":"isReferenced","value":["true"]}]'
);