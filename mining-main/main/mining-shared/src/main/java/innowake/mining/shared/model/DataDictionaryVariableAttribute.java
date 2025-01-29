/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model;

import innowake.mining.shared.entities.DataDictionaryPojo;

/**
 * The attribute of a variable within a certain scope of a {@link DataDictionaryPojo}.
 */
public enum DataDictionaryVariableAttribute {

	SQL_DATABASE_TABLES(DataDictionaryVariableScope.SQL_DATABASE, "tables"),
	NATURAL_DATABASE_DDM(DataDictionaryVariableScope.NATURAL_DATABASE, "ddm"),
	NATURAL_DATABASE_ADABAS_FILE(DataDictionaryVariableScope.NATURAL_DATABASE, "adabas_file"),
	NATURAL_DATABASE_ADABAS_SHORTNAME(DataDictionaryVariableScope.NATURAL_DATABASE, "adabas_shortname"),
	FILE_DATASET(DataDictionaryVariableScope.FILE, "dataset"),
	CICS_UI_MAPSET(DataDictionaryVariableScope.CICS_UI, "mapset"),
	CICS_UI_MAPNAME(DataDictionaryVariableScope.CICS_UI, "mapname"),
	NATURAL_UI_MAP(DataDictionaryVariableScope.NATURAL_UI, "map"),
	/* for OrientDB DataDictionaryEntry.otherScopeSource */
	OTHER_SOURCE(DataDictionaryVariableScope.OTHER, "source"),
	/* for OrientDB DataDictionaryEntry.otherScopeLink.name */
	OTHER_SCOPE(DataDictionaryVariableScope.OTHER, "scope");

	private final DataDictionaryVariableScope scope;
	private final String key;
	
	private DataDictionaryVariableAttribute(final DataDictionaryVariableScope scope, final String name) {
		this.scope = scope;
		this.key = name;
	}
	
	public DataDictionaryVariableScope getScope() {
		return scope;
	}
	
	public String getKey() {
		return key;
	}

}
