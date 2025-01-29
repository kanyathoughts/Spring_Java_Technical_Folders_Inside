/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model.aggregations;

import innowake.mining.shared.access.Table;
import innowake.mining.shared.model.FieldName;

import java.sql.Array;
import java.sql.SQLException;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
* Result with aggregated field values.
 *
 * @param <F> the set of valid field names
 */
public class AggregationResult<F> {

	private Map<F, Object> group = new HashMap<>();
	private Map<F, Object> fields = new HashMap<>();
	
	/**
	 * Gets the group map
	 *
	 * @return the group map
	 */
	public Map<F, Object> getGroup() {
		return group;
	}
	
	/**
	 * Sets the group map
	 *
	 * @param group the group map
	 */
	public void setGroup(final Map<F, Object> group) {
		this.group = group;
	}
	
	/**
	 * Gets the fields map
	 *
	 * @return the fields map
	 */
	public Map<F, Object> getFields() {
		return fields;
	}
	
	/**
	 * Sets the fields map
	 * 
	 * @param fields the fields map
	 */
	public void setFields(final Map<F, Object> fields) {
		this.fields = fields;
	}

	/**
	 * Converts a table to a list of {@link AggregationResult}
	 *
	 * @param table the table to convert
	 * @param request the aggregation request
	 * @param fieldClass the class of the field
	 * @return the value of the field
	 */
	public static <F extends Enum<F> & FieldName> List<AggregationResult<F>> getResultFromTable(final Table table, final AggregationRequest<F> request,
			final Class<F> fieldClass) {
		return table.stream().map(row -> {
				final Map<F, Object> fields = new EnumMap<>(fieldClass);
				final Map<F, Object> groups = new EnumMap<>(fieldClass);
				final AggregationResult<F> result = new AggregationResult<>();
				result.setFields(fields);
				result.setGroup(groups);
				request.getFields().keySet().forEach(fieldName -> mapRowToResult(row, fieldName, fields));
				request.getGroupBy().forEach(fieldName -> mapRowToResult(row, fieldName, groups));
				return result;
			}).collect(Collectors.toList());
	}

	private static <F extends Enum<F> & FieldName> void mapRowToResult(final Table.Row row, final F fieldName, final Map<F, Object> fields) {
		final Object value = row.get(fieldName.name().toLowerCase());
		if (value instanceof Array) {
			try {
				fields.put(fieldName, ((Array) value).getArray());
			} catch (final SQLException e) {
				throw new IllegalStateException("Error while parsing SQL Array", e);
			}
		} else {
			fields.put(fieldName, value);
		}
	}
}
