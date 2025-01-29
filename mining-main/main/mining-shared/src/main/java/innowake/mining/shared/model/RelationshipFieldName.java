/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model;

/**
 * Enumerate the Field Names available for Module Relationships
 */
public enum RelationshipFieldName implements FieldName {
	
	ID,
	RELATIONSHIP,
	DST_ID,
	SRC_ID,
	DST_NAME,
	SRC_NAME,
	DST_PROJECT_ID,
	SRC_PROJECT_ID,
	DST_TECHNOLOGY,
	SRC_TECHNOLOGY,
	DST_TYPE,
	SRC_TYPE,
	PROPERTY_DB_ACCESS_TYPE,
	PROPERTY_DB_ACCESS_OPERATION,
	SRC_LINKHASH,
	DST_LINKHASH,
	SRC_STORAGE,
	DST_STORAGE,
	TAXONOMY_ID;
}
