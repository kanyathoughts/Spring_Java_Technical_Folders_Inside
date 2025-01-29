/* Copyright (c) 2019 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.data;

import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;

import org.springframework.jdbc.core.RowMapper;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.model.Entity;

/**
 * Base class for row mappers, which handles filling the custom properties defined by the individual mining projects.
 * 
 * @param <T> the concrete type of the {@link Entity}
 */
public abstract class BaseRowMapper<T extends Entity> implements RowMapper<T> {

	@Nullable
	@Override
	public T mapRow(@Nullable final ResultSet resultSet, final int rowNum) throws SQLException {
		if (resultSet == null) {
			return null;
		}
		return map(resultSet, rowNum);
	}

	/**
	 * Convert the column names of a ResultSet to a Map of Strings with the column index as their value.
	 *
	 * @param resultSet ResultSet to extract metadata from.
	 * @return Map of column names to column index.
	 * @throws SQLException if a database access error occurs or this method is called on a closed result set
	 */
	public static Map<String, Integer> getColumnsMap(final ResultSet resultSet) throws SQLException {
		final ResultSetMetaData meta = resultSet.getMetaData();
		final Map<String, Integer> columnsMap = new HashMap<>();
		for (int column = 1; column <= meta.getColumnCount(); column++) {
			columnsMap.put(meta.getColumnName(column), column);
		}
		return columnsMap;
	}

	/**
	 * Implementations must implement this method to map each row of data in the ResultSet. 
	 * <p>
	 * This method should not call {@code next()} on
	 * the ResultSet; it is only supposed to map values of the current row.
	 * 
	 * @param rs the ResultSet to map (pre-initialized for the current row)
	 * @param rowNum the number of the current row
	 * @return the result object for the current row (may be {@code null})
	 * @throws SQLException if a SQLException is encountered getting column values (that is, there's no need to catch SQLException)
	 */
	protected abstract T map(final ResultSet rs, final int rowNum) throws SQLException;
}
