UPDATE AstNode SET children=[] WHERE children IS NULL;
ALTER PROPERTY AstNode.children NOTNULL TRUE;
ALTER PROPERTY AstNode.children DEFAULT [];

UPDATE AstNode SET properties={} WHERE properties IS NULL;
ALTER PROPERTY AstNode.properties NOTNULL TRUE;
ALTER PROPERTY AstNode.properties DEFAULT {};

UPDATE AstNode SET superTypes=[] WHERE superTypes IS NULL;
ALTER PROPERTY AstNode.superTypes NOTNULL TRUE;
ALTER PROPERTY AstNode.superTypes DEFAULT [];

ALTER CLASS DataDictionaryEntry STRICTMODE FALSE;

UPDATE DataDictionaryEntry SET scopeAttributes={} WHERE scopeAttributes IS NULL;
ALTER PROPERTY DataDictionaryEntry.scopeAttributes NOTNULL TRUE;
ALTER PROPERTY DataDictionaryEntry.scopeAttributes DEFAULT {};

UPDATE DataDictionaryEntry SET scopeLink=[] WHERE scopeLink IS NULL;
ALTER PROPERTY DataDictionaryEntry.scopeLink NOTNULL TRUE;
ALTER PROPERTY DataDictionaryEntry.scopeLink DEFAULT [];

ALTER CLASS DataDictionaryEntry STRICTMODE TRUE;

UPDATE DataDictionaryEntryScopeAttributes SET attributes={} WHERE attributes IS NULL;
ALTER PROPERTY DataDictionaryEntryScopeAttributes.attributes NOTNULL TRUE;
ALTER PROPERTY DataDictionaryEntryScopeAttributes.attributes DEFAULT {};


UPDATE DnaData SET files={} WHERE files IS NULL;
ALTER PROPERTY DnaData.files NOTNULL TRUE;
ALTER PROPERTY DnaData.files DEFAULT {};

UPDATE EffortSummary SET pricingSummaries=[] WHERE pricingSummaries IS NULL;
ALTER PROPERTY EffortSummary.pricingSummaries NOTNULL TRUE;
ALTER PROPERTY EffortSummary.pricingSummaries DEFAULT [];

UPDATE EffortSummary SET typeSummaries=[] WHERE typeSummaries IS NULL;
ALTER PROPERTY EffortSummary.typeSummaries NOTNULL TRUE;
ALTER PROPERTY EffortSummary.typeSummaries DEFAULT [];

UPDATE Module SET info={} WHERE info IS NULL;
ALTER PROPERTY Module.info NOTNULL TRUE;
ALTER PROPERTY Module.info DEFAULT {};

UPDATE Project SET configurations={} WHERE configurations IS NULL;
ALTER PROPERTY Project.configurations NOTNULL TRUE;

UPDATE Project SET searchOrders=[] WHERE searchOrders IS NULL;
ALTER PROPERTY Project.searchOrders NOTNULL TRUE;
ALTER PROPERTY Project.searchOrders DEFAULT [];

UPDATE SearchOrder SET targetPatterns=[] WHERE targetPatterns IS NULL;
ALTER PROPERTY SearchOrder.targetPatterns NOTNULL TRUE;
ALTER PROPERTY SearchOrder.targetPatterns DEFAULT [];