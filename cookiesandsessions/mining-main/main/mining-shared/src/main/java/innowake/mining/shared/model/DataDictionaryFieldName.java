/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.shared.model;

import innowake.mining.shared.entities.DataDictionaryPojo;

/**
 * Enum representing Field Names from {@link DataDictionaryPojo}.
 */
public enum DataDictionaryFieldName implements FieldName {
	
	PROJECT_ID,
	ID,
	DATA_ELEMENT_NAME,
	DESCRIPTION,
	FORMAT,
	LENGTH,
	OTHER_SCOPE_LINK,
	OTHER_SCOPE_SOURCE,
	CREATED_BY_USER_ID,
	UPDATED_BY_USER_ID,
	MODULE_TECHNOLOGY,
	MODULE_TYPE,
	SCOPE_LINK,
	SCOPE_ATTRIBUTES,
	IS_CANDIDATE,
	PIC_CLAUSE,
	DEFINED_LOCATION,
	STATE,
	IS_BUSINESS,
	FIELD_TRANSFORMATION,
	SOURCE_INPUT,
	TARGET_OUTPUT,
	IS_REFERENCED,
	FIELD_USAGE,
	TAXONOMY_ID;

}
