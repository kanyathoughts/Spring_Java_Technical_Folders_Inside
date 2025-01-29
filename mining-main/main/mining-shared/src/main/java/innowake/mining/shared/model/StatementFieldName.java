/*
 * Copyright  2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model;

/**
 * Enum representing Field Names from {@code StatementV2}.
 */
public enum StatementFieldName implements FieldName {
	ID,
	PROJECT_ID,
	MODULE_ID,
	TECHNOLOGY,
	STATEMENT_TYPE,
	TEXT,
	TEXT_LENGTH,
	CUSTOM_COMPLEXITY,
	DISTINCT_TABLES,
	HALSTEAD_COMPLEXITY,
	HALSTEAD_DIFFICULTY,
	SQL_LENGTH,
	TABLES,
	TAXONOMY_ID;
}
