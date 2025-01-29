/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.access.postgres;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.Arrays;

/**
 * Wrapper for an array specifying the corresponding SQL type for proper conversion during query execution.
 */
public class PgArray {
	
	private final String type;
	private final Object[] elements;
	
	/**
	 * Wraps an array to be used as argument for an SQL query.
	 * @param typeName Declaration of the SQL type of the array elements.
	 * @param elements Array content.
	 */
	public PgArray(final String typeName, final Object[] elements) {
		this.type = typeName;
		this.elements = elements;
	}
	
	/**
	 * Gets the SQL type.
	 * @return Declaration of the SQL type of the array elements.
	 */
	public String getTypeName() {
		return type;
	}
	
	/**
	 * Gets the array elements.
	 * @return Array content.
	 */
	public Object[] getElements() {
		return elements;
	}
	
	/**
	 * Creates a JBDC array.
	 * @param connection Database connection.
	 * @return JDBC array object.
	 * @throws SQLException  If a database error occurs.
	 */
	public java.sql.Array toJdbcArray(final Connection connection) throws SQLException {
		return connection.createArrayOf(type, elements);
	}
	
	@Override
	public String toString() {
		return type + Arrays.asList(elements).toString();
	}
	
}