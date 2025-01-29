-- additional variable scopes for Natural
INSERT INTO DataDictionaryVariableScopeEnum SET name="NATURAL_DATABASE";
INSERT INTO DataDictionaryVariableScopeEnum SET name="NATURAL_UI";

-- changed variable scopes for Cobol
UPDATE DataDictionaryVariableScopeEnum SET name="SQL_DATABASE" where name = "DATABASE";
UPDATE DataDictionaryVariableScopeEnum SET name="COBOL_UI" where name = "UI";

ALTER CLASS DataDictionaryEntry STRICTMODE FALSE;

-- field format is no longer an enum
CREATE PROPERTY DataDictionaryEntry.format STRING;
UPDATE DataDictionaryEntry SET format = formatLink.name;

DROP PROPERTY DataDictionaryEntry.formatLink;
UPDATE DataDictionaryEntry REMOVE formatLink;

DELETE VERTEX FROM DataDictionaryVariableFormatEnum;
DROP CLASS DataDictionaryVariableFormatEnum;

-- all scope properties are now stored as maps
CREATE CLASS DataDictionaryEntryScopeAttributes EXTENDS V;
CREATE PROPERTY DataDictionaryEntryScopeAttributes.attributes EMBEDDEDMAP STRING;
CREATE PROPERTY DataDictionaryEntry.scopeAttributes EMBEDDEDMAP DataDictionaryEntryScopeAttributes;

UPDATE DataDictionaryEntry SET scopeAttributes["SQL_DATABASE"] = { "@class": "DataDictionaryEntryScopeAttributes", attributes: { tables: tables } } WHERE "SQL_DATABASE" IN scopeLink.name;
UPDATE DataDictionaryEntry SET scopeAttributes["FILE"] = { "@class": "DataDictionaryEntryScopeAttributes", attributes: { dataset: dataset } } WHERE "FILE" IN scopeLink.name;
UPDATE DataDictionaryEntry SET scopeAttributes["COBOL_UI"] = { "@class": "DataDictionaryEntryScopeAttributes", attributes: { mapset: mapset, mapname: mapname } } WHERE "COBOL_UI" IN scopeLink.name;
UPDATE DataDictionaryEntry SET scopeAttributes["OTHER"] = { "@class": "DataDictionaryEntryScopeAttributes", attributes: attributes } WHERE "OTHER" IN scopeLink.name;

DROP PROPERTY DataDictionaryEntry.dataset;
UPDATE DataDictionaryEntry REMOVE dataset;
DROP PROPERTY DataDictionaryEntry.tables;
UPDATE DataDictionaryEntry REMOVE tables;
DROP PROPERTY DataDictionaryEntry.mapset;
UPDATE DataDictionaryEntry REMOVE mapset;
DROP PROPERTY DataDictionaryEntry.mapname;
UPDATE DataDictionaryEntry REMOVE mapname;
DROP PROPERTY DataDictionaryEntry.attributes;
UPDATE DataDictionaryEntry REMOVE attributes;

-- more specific name for "source"
ALTER PROPERTY DataDictionaryEntry.source NAME otherScopeSource;

-- new AstNode super type for data fields
UPDATE AstNode SET superTypes = set(superTypes, "FieldDefinition") where type = "CobolDataField";

ALTER CLASS DataDictionaryEntry STRICTMODE TRUE;