/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model;

import innowake.mining.shared.entities.DataDictionaryPojo;

/**
 * The scope of a variable of a {@link DataDictionaryPojo}.
 */
public enum DataDictionaryVariableScope {

	FILE,
	SQL_DATABASE,
	NATURAL_DATABASE,
	CICS_UI,
	NATURAL_UI,
	PARAMETER,
	OTHER;
	
}
