/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.datapoints.annotations.MiningDataType;
import innowake.mining.shared.springdata.annotations.Entity;

@Entity(name = "StatementTypeEnum")
@MiningDataType(name = "StatementType")
public enum StatementType implements TypeIdentifier {

	EXEC_ADABAS,
	UPDATE,
	CREATE_CONSTRAINT,
	ALTER_INDEX,
	DROP_FOREIGN_KEY,
	LOCK_TABLE,
	OPEN,
	EXEC_CICS,
	MERGE,
	CREATE_CHECK_CONSTRAINT,
	ALTER_CONSTRAINT,
	DROP_INDEX,
	CLOSE,
	CONDITIONAL,
	EXEC_PROC,
	DELETE,
	CREATE_FUNCTION,
	ALTER_CHECK_CONSTRAINT,
	DROP_CONSTRAINT,
	FETCH,
	COMMIT,
	EXEC_REXX,
	CREATE_TABLE,
	CREATE_PROCEDURE,
	ALTER_FUNCTION,
	DROP_CHECK_CONSTRAINT,
	EXECUTE,
	ROLLBACK,
	ADAPREP,
	EXEC_SQL,
	CREATE_VIEW,
	CREATE_SYNONYM,
	ALTER_TABLE,
	ALTER_PROCEDURE,
	DROP_FUNCTION,
	ALLOCATE_CURSOR,
	SET,
	DB_ACCESS_TYPE,
	EXEC_UNKNOWN,
	CREATE_PRIMARY_KEY,
	ALTER_VIEW,
	DROP_TABLE,
	DROP_PROCEDURE,
	ASSOCIATE_LOCATOR,
	PREPARE,
	ENTRY,
	SELECT,
	CREATE_FOREIGN_KEY,
	ALTER_PRIMARY_KEY,
	DROP_VIEW,
	GRANT,
	WHENEVER,
	UNKNOWN,
	EXEC,
	INSERT,
	CREATE_INDEX,
	ALTER_FOREIGN_KEY,
	DROP_PRIMARY_KEY,
	DECLARE_TABLE,
	DECLARE_SCHEMA,
	RDB_DATABASE,
	DISPLAY,
	CALL,
	DECLARE_TEMP_TABLE,
	VALUES,
	FTP,
	DECLARE_ALIAS,
	DECLARE_TRANSACTION,
	EXECUTE_IMMEDIATE,
	CREATE_TRIGGER,
	GETERROR;
	
	@Nullable private final String name;

	private StatementType() {
		this.name = null;
	}

	/**
	 * Returns the StatementType given a statement string.
	 * <p>
	 * The comparison is done case-insensitive.
	 *
	 * @param name the statement string associated with.
	 * @return the StatementType
	 */
	public static StatementType fromName(final String name) {
		final String noSpaceName = name.replaceAll("\\s+","_");
		for (final StatementType value : values()) {
			if (value.name != null && value.name.equalsIgnoreCase(noSpaceName) || value.name().equalsIgnoreCase(noSpaceName)) {
				return value;
			}
		}
		throw new IllegalArgumentException("No enum constant " + name + " available");
	}
}
