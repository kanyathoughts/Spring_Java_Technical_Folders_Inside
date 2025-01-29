CREATE CLASS HasBusinessRule EXTENDS Reference;
CREATE PROPERTY Annotation.in_HasBusinessRule LINKLIST HasBusinessRule;
CREATE PROPERTY DataDictionaryEntry.out_HasBusinessRule LINKLIST HasBusinessRule;
CREATE PROPERTY HasBusinessRule.out LINK DataDictionaryEntry (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY HasBusinessRule.in LINK Annotation (NOTNULL, MANDATORY TRUE);
