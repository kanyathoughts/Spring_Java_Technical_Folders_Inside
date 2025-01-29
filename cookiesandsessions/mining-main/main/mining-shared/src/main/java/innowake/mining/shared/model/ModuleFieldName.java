/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model;

/**
 * Enumerate the Field Name available for Modules
 */
public enum ModuleFieldName implements FieldName {
	
	ID,
	UID,
	NAME,
	PROJECT_ID,
	TECHNOLOGY,
	TYPE,
	REQUIRES_REVIEW,
	STORAGE,
	CREATOR,
	IDENTIFICATION,
	LINES_OF_CODE,
	LINES_OF_COMMENT,
	LINES_OF_DEAD_CODE,
	COMPLEXITY,
	ERRORS,
	REPRESENTATION,
	SQL_STATEMENTS,
	CONTAINING_MODULE_ID,
	CONTAINING_MODULE_NAME,
	PHYSICAL_LINES_OF_CODE,
	TAXONOMY_ID,
	CATEGORIES,
	INTERFACE,
	INBOUND,
	OUTBOUND,
	ORIGIN;
}
